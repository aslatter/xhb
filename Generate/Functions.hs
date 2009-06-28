module Generate.Functions where

import Data.Char
import Data.Maybe

import Control.Applicative

import Control.Exception(assert)

import Data.XCB

import HaskellCombinators
import Generate(valueParamName,mapAlt,xImport,mapIdents,fieldName,fieldType)
import Generate.Monad
import Generate.Facts
import Generate.Util

import Control.Monad.Reader
import Control.Monad.Maybe

-- Builds a function for every request in the module
-- Hopefully I'm not duplicating too much between here
-- and the types generation module.

-- | Returns the name of the Haskell module containing the type
-- declarations for a given XCB module.
typesModName :: GenXHeader a -> String
typesModName = typesModuleName . interCapsName

-- | Returns the name of the Haskell module containing the function
-- definitions for a given XCB module.
functionsModName :: GenXHeader a -> String
functionsModName = functionsModuleName . interCapsName

-- | Returns the name of an X module in InterCaps.
interCapsName :: GenXHeader a -> String
interCapsName xhd = case xheader_name xhd of
                      Nothing -> ensureUpper $ xheader_header xhd
                      Just name -> name

ensureLower [] = []
ensureLower (x:xs) = toLower x : xs

-- | Given a list of X modules, returns a list of generated Haskell modules
-- which contain the developer friendly functions for using XHB.
functionsModules :: [XHeader] -> [HsModule]
functionsModules xs = map go transed
 
    where transed =  standardTranslations xs

          go :: HXHeader -> HsModule
          go xhd = functionsModule transed xhd

-- | Generates the Haskell functions for using the functionality
-- of the passed in X module.
functionsModule :: [HXHeader] -> HXHeader -> HsModule
functionsModule xs xhd | isCoreModule xhd = buildCore xhd
                       | otherwise = buildExtension xs xhd

-- | Retuns 'True' if the X module is NOT for an extension.
isCoreModule = isNothing . xheader_xname

buildExtension :: [HXHeader] -> HXHeader -> HsModule
buildExtension xs xhd = 
    let emptyModule = newExtensionModule xhd
        rs = requests xhd
        fns = declareFunctions xhd rs
        extId = declareExtensionId xhd
        imFns = doImports xs xhd
    in moveExports $ applyMany (extId ++ fns ++ imFns) emptyModule

declareExtensionId :: HXHeader -> [HsModule -> HsModule]
declareExtensionId xhd =
    [addDecl $ mkTypeSig extFnName [] (mkTyCon "ExtensionId")
    ,addDecl $ mkSimpleFun extFnName [] $
             mkStringLit $ fromJust $ xheader_xname xhd
    ,addExport $ mkExportVar extFnName
    ]

 where extFnName = "extension"

doImports :: [HXHeader] -> HXHeader -> [HsModule -> HsModule]
doImports xs xhd =
    let decs = xheader_decls xhd
    in mapMaybe go decs
    
  where go :: HXDecl -> Maybe (HsModule -> HsModule)
        go (XImport name) = return $ xImport xs xhd name
        go _ = Nothing

-- | Builds a haskel functions module for the passed in xml
-- description.  Assumes it is not for extension requests.
buildCore :: HXHeader -> HsModule
buildCore xhd =
    let emptyModule = newCoreModule xhd
        rs = requests xhd
        fns = declareFunctions xhd rs
    in moveExports $ applyMany fns emptyModule

-- | moves entire-module exports to end of export list
moveExports :: HsModule -> HsModule
moveExports =
    modifyExports $ \exports ->
    let (modExports, otherExports) = filterAccum isModExport exports
    in otherExports ++ modExports

modifyExports f mod =
    case getExports mod of
      Nothing -> mod
      (Just exs) -> setExports (Just $ f exs) mod
                         

applyMany = foldr (flip (.)) id

-- Creates a nearly empty Haskell module for the passed-in
-- X module.  Also inserts standard Haskell imports.
newCoreModule :: HXHeader -> HsModule
newCoreModule xhd = 
    let name = functionsModName xhd
        mod = mkModule name
    in exportTypesMod xhd $ doQualImports $ doImports mod
 where doImports = applyMany $ map (addImport . mkImport) $
             [typesModName xhd
             , packagePrefix ++ ".Connection.Internal"
             , packagePrefix ++ ".Shared"
             ,"Data.Binary.Put"
             ,"Control.Concurrent.STM"
             ,"Foreign.C.Types"
             ,"Data.Word"
             ,"Data.Int"
             ,"Data.Binary.Get"
             ]

       doQualImports = addImport $ mkQualImport $
                         packagePrefix ++ ".Connection.Types"

newExtensionModule :: HXHeader -> HsModule
newExtensionModule xhd =
    let name = functionsModName xhd
        mod = mkModule name
    in exportTypesMod xhd $ doHidingImports $ doSomeImports $ doImports mod
 where doImports = applyMany $ map (addImport . mkImport) $
             [typesModName xhd
             , packagePrefix ++ ".Connection.Internal"
             , packagePrefix ++ ".Connection.Extension"
             , packagePrefix ++ ".Connection.Types"
             , "Control.Concurrent.STM"
             , "Foreign.C.Types"
             , "Data.Word"
             , "Data.Int"
             , "Data.Binary.Get"
             ]

       doSomeImports = addImport $ mkSomeImport "Data.Binary.Put" ["runPut"]
       doHidingImports = addImport $ mkHidingImport (packagePrefix ++ ".Shared") ["Event", "Error"]

exportTypesMod = addExport . mkExportModule . typesModName

connTyName = packagePrefix ++ ".Connection.Types.Connection"


makeReceipt :: RequestInfo -> [HsStmt]
makeReceipt req
    | not (hasReply req) = empty

    | unaryReply req = return $ mkBinding $
       hsApp (mkVar "newEmptyReceipt") $ hsParen $
       hsApp (mkVar "runGet") $ hsParen $
       hsInfixApp (mkVar unaryReplyAccessorName)
                  (mkQOpIdent "fmap")
                  (mkVar "deserialize")

    | otherwise = return $ mkBinding $
       mkVar "newDeserReceipt"

 where
   mkBinding = mkGenerator (hsPTuple [mkPVar "receipt", mkPVar "rReceipt"])
   
   unaryReplyAccessorName = accessor elemName name
    where name = replyName (request_name req)
          elemName = maybe (error $ "Failure in mkReceiptForReply! " ++ show req) id $
                     firstReplyElem req >>= fieldName


-- send rReceipt, bu still return receipt
sendRequest :: RequestInfo -> [HsStmt]
sendRequest req | hasReply req = map hsQualifier
                   [foldl1 hsApp $ map mkVar $
                    ["sendRequestWithReply"
                    ,"c"
                    ,"chunk"
                    ,"rReceipt"
                    ]
                   ,mkVar "return" `hsApp` mkVar "receipt"
                   ]
                | otherwise = map hsQualifier $
                    return $ (mkVar "sendRequest" `hsApp` mkVar "c")
                          `hsApp` mkVar "chunk"

-- account for unary/nullary reply case
resultType :: RequestInfo -> HsType
resultType req | unaryReply req =
                      receiptType $ fromJust $ fieldType $ fromJust $ firstReplyElem req
               | hasReply req = receiptType $ replyType req
               | otherwise = foldr1 hsTyApp $
                             [mkTyCon "IO"
                             ,unit_tycon
                             ]

receiptType :: HsType -> HsType
receiptType typ = foldr1 hsTyApp $
                  [mkTyCon "IO"
                  ,mkTyCon "Receipt"
                  ,typ]

replyType :: RequestInfo -> HsType
replyType = mkTyCon . replyNameFromInfo


-- | Declares Haskell functions for an X module.
declareFunctions :: HXHeader -> [RequestInfo] -> [HsModule -> HsModule]
declareFunctions xhd rInfos =
  map (declareFunction (not $ isCoreModule xhd)) rInfos

-- for core requests, we can do the short form and long form
-- because we don't have to import any other modules
-- | Handles a single request in the core functions module.
declareFunction :: Bool -> RequestInfo -> (HsModule -> HsModule)
declareFunction ext req =
  applyMany
   [addDecl typDeclaration
   ,addDecl fnDeclaration
   ,addExport $ mkExportAbs fnName
   ]
 where fnName = fnNameFromRequest req

       fields = requestFields req
       fieldCount = length fields

       bigCount = 3

       shortMode = fieldCount < bigCount

       typDeclaration :: HsDecl
       typDeclaration | shortMode = shortTypDec
                      | otherwise = longTypDec

       fnDeclaration :: HsDecl
       fnDeclaration | shortMode = shortFnDec
                     | otherwise = longFnDec

       shortTypDec, longTypDec :: HsDecl
       shortTypDec = mkTypeSig fnName [] shortTyp
       longTypDec  = mkTypeSig fnName [] longType

       shortTyp = 
         let fieldTypes = fieldsToTypes fields
         in foldr1 hsTyFun $
             mkTyCon connTyName : fieldTypes ++ [resultType req]
                                

       longType = 
         foldr1 hsTyFun $
                [mkTyCon connTyName
                ,mkTyCon $ request_name req
                ,resultType req
                ]


       shortFnDec = mkSimpleFun fnName
                    (map mkPVar shortArgs)
                    (hsDo fnBody)

       longFnDec = mkSimpleFun fnName
                   (map mkPVar ["c", "req"])
                   (hsDo fnBody)

       shortArgs = "c" : fieldsToArgNames fields


       -- constructor plus args
       shortRequestExpr :: HsExp
       shortRequestExpr = 
           foldl1 hsApp $ constructor : map mkVar (fieldsToArgNames fields)

       -- TODO: share constructor name between
       -- generation condebases.
       constructor :: HsExp
       constructor = hsCon . mkUnQName $ "Mk" ++ request_name req

       fnBody :: [HsStmt]
       fnBody = concat
                [ makeReceipt req
                , buildRequest
                , serializeRequest
                , sendRequest req
                ]

       buildRequest | shortMode = return $ mkLetStmt
                                 (mkPVar "req")
                                 shortRequestExpr
                    | otherwise = empty

       serializeRequest
           | ext = [ mkGenerator (mkPVar "putAction")
                           (foldl1 hsApp $ map mkVar $
                              ["serializeExtensionRequest"
                              ,"c"
                              ,"req"
                              ]
                           )
                   , mkLetStmt (mkPVar "chunk")
                     (mkVar "runPut" `hsApp` mkVar "putAction")
                   ]
           | otherwise = [mkLetStmt (mkPVar "chunk")
                              (applyManyExp
                               [mkVar "runPut"
                               ,mkVar "serialize" `hsApp` mkVar "req"
                               ])
                         ]


-- | Fold Haskell expressions together in a right-fold fashion
applyManyExp [] = undefined
applyManyExp [x] = x
applyManyExp (x:xs) = hsApp x $ hsParen $ applyManyExp xs

-- | Maps the fields of a X-struct into argument names to be used
-- in an arg-list for a Haskell function
fieldsToArgNames :: [HStructElem] -> [String]
fieldsToArgNames = map mapIdents . mapMaybe fieldToArgName

fieldToArgName :: HStructElem -> Maybe String
fieldToArgName = fieldName

-- | The types corresponding to the args from "fieldsToArgNames".
fieldsToTypes :: [HStructElem] -> [HsType]
fieldsToTypes = mapMaybe fieldType


-- | Extracts the requests from an X module.
requests :: HXHeader -> [RequestInfo]
requests = mapMaybe go . xheader_decls
 where go (XRequest name code elems reply) = return $
          RequestInfo name code elems reply
       go _ = empty

data RequestInfo = RequestInfo
    {request_name :: Name
    ,request_code :: Int
    ,request_elems :: [HStructElem]
    ,request_reply :: Maybe HXReply
    } deriving Show

-- | Extracts only the fields in a request that must be specified
-- by the library end-user.  That is, padding and such is excluded.
requestFields :: RequestInfo -> [HStructElem]
requestFields = filter go . request_elems
 where go List{} = True
       go SField{} = True
       go ValueParam{} = True
       go _ = False

-- | Returns true if a request has a reply
hasReply :: RequestInfo -> Bool
hasReply = not . isNothing . request_reply


-- | Return true if the reply is a unary reply - as in, has
-- on one element
unaryReply :: RequestInfo -> Bool
unaryReply RequestInfo{request_reply = Just xs}
    = 1 == length (filter interestingField xs)
unaryReply _ = False

-- | Returns the first StructElem in the reply, if there is
-- one.
firstReplyElem :: RequestInfo -> Maybe HStructElem
firstReplyElem = listToMaybe . filter interestingField
                 . maybe [] id . request_reply


interestingField :: GenStructElem a -> Bool
interestingField Pad{} = False
interestingField ExprField{} = False
interestingField _ = True


-- | For a request, returns what the end-user Haskell function
-- is to be named
fnNameFromRequest :: RequestInfo -> String
fnNameFromRequest = ensureLower . request_name

-- | For a request, returns the name of the Haskell type constructor
-- corresponding to its reply.
replyNameFromInfo :: RequestInfo -> String
replyNameFromInfo req = assert (hasReply req) $
                        replyName $ request_name req
