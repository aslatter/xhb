module Generate.Functions(functionsModules) where

import Data.Char
import Data.Maybe

import Control.Applicative

import Control.Exception(assert)

import Data.XCB
import Language.Haskell.Syntax

import HaskellCombinators
import Generate(valueParamName)
import Generate.Monad(mapTyNames)
import Generate.Facts

-- Builds a function for every request in the module
-- Hopefully I'm not duplicating too much between here
-- and the types generation module.

-- | Returns the name of the Haskell module containing the type
-- declarations for a given XCB module.
typesModName :: XHeader -> String
typesModName xhd = typesModuleName $ interCapsName xhd

-- | Returns the name of the Haskell module containing the function
-- definitions for a given XCB module.
functionsModName :: XHeader -> String
functionsModName xhd = functionsModuleName $ interCapsName xhd

-- | Returns the name of an X module in InterCaps.
interCapsName :: XHeader -> String
interCapsName xhd = case xheader_name xhd of
                      Nothing -> ensureUpper $ xheader_header xhd
                      Just name -> name

ensureUpper [] = []
ensureUpper (x:xs) = (toUpper x) : xs

ensureLower [] = []
ensureLower (x:xs) = (toLower x) : xs

-- | Given a list of X modules, returns a list of generated Haskell modules
-- which contain the developer friendly functions for using XHB.
functionsModules :: [XHeader] -> [HsModule]
functionsModules = map functionsModule

-- | Generates the Haskell functions for using the functionality
-- of the passed in X module.
functionsModule :: XHeader -> HsModule
functionsModule xhd | isCoreModule xhd = buildCore xhd
                    | otherwise = buildExtension xhd

-- | Retuns 'True' if the X module is NOT for an extension.
isCoreModule = isNothing . xheader_xname

buildExtension :: XHeader -> HsModule
buildExtension xhd =
    let emptyModule = newExtensionModule xhd
        rs = requests xhd
        fns = declareExtensionFunctions rs
    in applyMany fns emptyModule

-- | Builds a haskel functions module for the passed in xml
-- description.  Assumes it is not for extension requests.
buildCore :: XHeader -> HsModule
buildCore xhd = 
    let emptyModule = newCoreModule xhd
        rs = requests xhd
        fns = declareCoreFunctions rs
    in applyMany fns emptyModule

applyMany = foldr (flip (.)) id

-- Creates a nearly empty Haskell module for the passed-in
-- X module.  Also inserts standard Haskell imports.
newCoreModule :: XHeader -> HsModule
newCoreModule xhd = 
    let name = functionsModName xhd
        mod = mkModule name
    in doHidingImports $ doImports mod
 where doImports = applyMany $ map (addImport . mkImport) $
             [typesModName xhd
             , packagePrefix ++ ".Connection.Internal"
             , packagePrefix ++ ".Shared"
             ,"Data.Binary.Put"
             ,"Control.Concurrent.STM"
             ,"Foreign.C.Types"
             ]

       doHidingImports = addImport $ mkQualImport $
                         packagePrefix ++ ".Connection.Types"

newExtensionModule :: XHeader -> HsModule
newExtensionModule xhd =
    let name = functionsModName xhd
        mod = mkModule name
    in doSomeImports $ doImports mod
 where doImports = applyMany $ map (addImport . mkImport) $
             [typesModName xhd
             , packagePrefix ++ ".Connection.Internal"
             , packagePrefix ++ ".Connection.Extension"
             , packagePrefix ++ ".Connection.Types"
             , packagePrefix ++ ".Shared"
             ,"Control.Concurrent.STM"
             ]

       doSomeImports = addImport $ mkSomeImport "Data.Binary.Put" ["runPut"]

connTyName = packagePrefix ++ ".Connection.Types.Connection"

declareExtensionFunctions :: [RequestInfo] -> [HsModule -> HsModule]
declareExtensionFunctions = map declareExtensionFunction

declareExtensionFunction req = applyMany
   [addDecl typDeclaration
   ,addDecl fnDeclaration
   ,addExport $ mkExportAbs fnName
   ]

 where fnName = fnNameFromRequest req

       fields = requestFields req
       noFields = null fields

       typDeclaration = mkTypeSig fnName [] typeDec

       typeDec | noFields = mkTyCon connTyName `HsTyFun` resultType req
               | otherwise = foldr1 HsTyFun $
                             [mkTyCon connTyName 
                             ,mkTyCon $ request_name req
                             ,resultType req
                             ]

       fnDeclaration = mkSimpleFun fnName
                       (map mkPVar fnArgs)
                       (HsDo fnBody)

       fnArgs :: [String]
       fnArgs | noFields = ["c"]
              | otherwise = ["c","req"]

       fnBody :: [HsStmt]
       fnBody = concat
                [ makeReceipt req
                , buildRequest
                , serializeRequest
                , sendRequest req
                ]

       buildRequest | noFields = return $ mkLetStmt
                                 (mkPVar "req")
                                 (HsCon . mkUnQName $ "Mk" ++ request_name req)
                    | otherwise = empty

       serializeRequest
           = [ mkGenerator (mkPVar "putAction")
                           (foldl1 HsApp $ map mkVar $
                              ["serializeExtensionRequest"
                              ,"c"
                              ,"req"
                              ]
                           )
             , mkLetStmt (mkPVar "chunk")
                         (mkVar "runPut" `HsApp` mkVar "putAction")
             ]


makeReceipt :: RequestInfo -> [HsStmt]
makeReceipt req | hasReply req = return $
                   mkGenerator (mkPVar "receipt")
                               (mkVar "newEmptyTMVarIO")
                | otherwise = empty

sendRequest :: RequestInfo -> [HsStmt]
sendRequest req | hasReply req = map HsQualifier
                   [foldl1 HsApp $ map mkVar $
                    ["sendRequestWithReply"
                    ,"c"
                    ,"chunk"
                    ,"receipt"
                    ]
                   ,mkVar "return" `HsApp` mkVar "receipt"
                   ]
                | otherwise = map HsQualifier $
                    return $ (mkVar "sendRequest" `HsApp` mkVar "c")
                          `HsApp` mkVar "chunk"

getByteOrder :: HsStmt
getByteOrder = mkLetStmt (mkPVar "bo")
               (mkVar "byteOrderFromConn" `HsApp` mkVar "c")


resultType :: RequestInfo -> HsType
resultType req | hasReply req = foldr1 HsTyApp $
                                [mkTyCon "IO"
                                ,mkTyCon "Receipt"
                                ,replyType req
                                ]
               | otherwise = foldr1 HsTyApp $
                             [mkTyCon "IO"
                             ,unit_tycon
                             ]

replyType :: RequestInfo -> HsType
replyType req = mkTyCon $ replyNameFromInfo req


-- | Declares Haskell functions for a "core" X module.  And by core I
-- mean the "xproto.xml" module.
declareCoreFunctions :: [RequestInfo] -> [HsModule -> HsModule]
declareCoreFunctions = map declareCoreFunction

-- for core requests, we can do the short form and long form
-- because we don't have to import any other modules
-- | Handles a single request in the core functions module.
declareCoreFunction :: RequestInfo -> HsModule -> HsModule
declareCoreFunction req = applyMany
   [addDecl typDeclaration
   ,addDecl fnDeclaration
   ,addExport $ mkExportAbs fnName
   ]
 where fnName = fnNameFromRequest req

       fields = requestFields req
       fieldCount = length fields

       bigCount = 3

       typDeclaration :: HsDecl
       typDeclaration | fieldCount < bigCount = shortTypDec
                      | otherwise = longTypDec

       fnDeclaration :: HsDecl
       fnDeclaration | fieldCount < bigCount = shortFnDec
                     | otherwise = longFnDec

       shortTypDec, longTypDec :: HsDecl
       shortTypDec = mkTypeSig fnName [] shortTyp
       longTypDec = mkTypeSig fnName [] longType

       shortTyp = foldr1 HsTyFun $
         (mkTyCon connTyName) : fieldsToTypes fields ++ [resultType req]
                                

       longType = foldr1 HsTyFun $
                  [mkTyCon connTyName
                  ,mkTyCon  $ request_name req
                  ] ++ [resultType req]


       shortFnDec = mkSimpleFun fnName
                    (map mkPVar shortArgs)
                    (HsDo shortBody)

       longFnDec = mkSimpleFun fnName
                   (map mkPVar ["c", "req"])
                   (HsDo longBody)

       shortArgs = "c" : fieldsToArgNames fields


       shortBody :: [HsStmt]
       shortBody = concat
                   [ -- make the receipt if we need one
                     makeReceipt req

                     -- serialize the request to the var "chunk"
                   , [ getByteOrder
                     , mkLetStmt (mkPVar "chunk")
                        (applyManyExp
                         [mkVar "runPut"
                         ,mkVar "serialize" `HsApp` mkVar "bo"
                         ,shortRequestExpr
                         ])
                      ]

                     -- send the request
                   , sendRequest req
                   ]

       longBody :: [HsStmt]
       longBody = concat
                  [ makeReceipt req

                  , [ getByteOrder
                    , mkLetStmt (mkPVar "chunk")
                      (applyManyExp
                       [mkVar "runPut"
                       ,mkVar "serialize" `HsApp` mkVar "bo" `HsApp` mkVar "req"
                       ])
                    ]                       

                  , sendRequest req
                  ]

                         
       -- constructor plus args
       shortRequestExpr :: HsExp
       shortRequestExpr = 
           foldl1 HsApp $ constructor : map mkVar (fieldsToArgNames fields)

       -- TODO: share constructor name between
       -- generation condebases.
       constructor :: HsExp
       constructor = mkVar $ ("Mk"++) $ request_name req


-- | Fold Haskell expressions together in a right-fold fashion
applyManyExp [] = undefined
applyManyExp [x] = x
applyManyExp (x:xs) = HsApp x $ HsParen $ applyManyExp xs

-- | Maps the fields of a X-struct into argument names to be used
-- in an arg-list for a Haskell function
fieldsToArgNames :: [StructElem] -> [String]
fieldsToArgNames = mapMaybe fieldToArgName

fieldToArgName :: StructElem -> Maybe String
fieldToArgName (List name _ _) = return name
fieldToArgName (SField name _) = return name
fieldToArgName (ValueParam _ mname _) = return $ valueParamName mname
fieldToArgName _ = empty

-- | The types corresponding to the args from "fieldsToArgNames".
fieldsToTypes :: [StructElem] -> [HsType]
fieldsToTypes = mapMaybe fieldToType

fieldToType :: StructElem -> Maybe HsType
fieldToType (SField _ typ) = return . mkTyCon $ simpleType typ
fieldToType (List _ typ _) = return $ list_tycon `HsTyApp` mkTyCon (simpleType typ)
fieldToType (ValueParam typ _ _) = return $ mkTyCon "ValueParam" `HsTyApp` mkTyCon (simpleType typ)
fieldToType _ = empty

-- | Converts a 'Type' to a 'String' usable by 'mkTyCon'.
-- Currently fails for qualified types.
simpleType :: Type -> String
simpleType QualType{} = error "simpleType: Unexpected qualified type"
simpleType (UnQualType typ) = mapTyNames typ

-- | Extracts the requests from an X module.
requests :: XHeader -> [RequestInfo]
requests = mapMaybe go . xheader_decls
 where go (XRequest name code elems reply) = return $
          RequestInfo name code elems reply
       go _ = empty

data RequestInfo = RequestInfo
    {request_name :: Name
    ,request_code :: Int
    ,request_elems :: [StructElem]
    ,request_reply :: Maybe XReply
    }

-- | Extracts only the fields in a request that must be specified
-- by the library end-user.  That is, padding and such is excluded.
requestFields :: RequestInfo -> [StructElem]
requestFields = filter go . request_elems
 where go List{} = True
       go SField{} = True
       go ValueParam{} = True
       go _ = False

-- | Returns true if a request has a reply
hasReply :: RequestInfo -> Bool
hasReply = not . isNothing . request_reply

-- | For a request, returns what the end-user Haskell function
-- is to be named
fnNameFromRequest :: RequestInfo -> String
fnNameFromRequest = ensureLower . request_name

-- | For a request, returns the name of the Haskell type constructor
-- corresponding to its reply.
replyNameFromInfo :: RequestInfo -> String
replyNameFromInfo req = assert (hasReply req) $
                        replyName $ request_name req
