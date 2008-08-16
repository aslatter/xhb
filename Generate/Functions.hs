module Generate.Functions(functionsModules) where

import Data.Char
import Data.Maybe

import Control.Applicative

import Control.Exception(assert)

import Data.XCB
import Language.Haskell.Syntax

import HaskellCombinators
import Generate(valueParamName, mapTyNames)

-- Builds a function for every request in the module
-- Hopefully I'm not duplicating too much between here
-- and the types generation module.

typesModuleName :: XHeader -> String
typesModuleName xhd = "XHB.Gen." ++ interCapsName xhd ++ ".Types"

functionsModuleName :: XHeader -> String
functionsModuleName xhd = "XHB.Gen." ++ interCapsName xhd

interCapsName :: XHeader -> String
interCapsName xhd = case xheader_name xhd of
                      Nothing -> ensureUpper $ xheader_header xhd
                      Just name -> name

ensureUpper [] = []
ensureUpper (x:xs) = (toUpper x) : xs

ensureLower [] = []
ensureLower (x:xs) = (toLower x) : xs

functionsModules :: [XHeader] -> [HsModule]
functionsModules = mapMaybe functionsModule

functionsModule :: XHeader -> Maybe HsModule
functionsModule xhd | isCoreModule xhd = return $ buildCore xhd
                    | otherwise = Nothing

isCoreModule = isNothing . xheader_xname

-- | Builds a haskel functions module for the passed in xml
-- description.  Assumes it is not for extension requests.
buildCore :: XHeader -> HsModule
buildCore xhd = 
    let emptyModule = newModule xhd
        rs = requests xhd
        fns = declareCoreFunctions rs
    in applyMany fns emptyModule

applyMany = foldr (flip (.)) id

-- name + standard imports
newModule :: XHeader -> HsModule
newModule xhd = 
    let name = functionsModuleName xhd
        mod = mkModule name
    in doImports mod
 where doImports = applyMany $ map (addImport . mkImport) $
             [typesModuleName xhd
             ,"XHB.Connection.Internal"
             ,"XHB.Connection.Types"
             ,"XHB.Shared"
             ,"Data.Binary.Put"
             ,"Control.Concurrent.STM"
             ,"Foreign.C.Types"
             ]

-- each request adds stuff to a mdule
declareCoreFunctions :: [RequestInfo] -> [HsModule -> HsModule]
declareCoreFunctions = map declareCoreFunction

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
         (mkTyCon "Connection") : fieldsToTypes fields ++ [resultType]
                                

       longType = foldr1 HsTyFun $
                  [mkTyCon "Connection"
                  ,mkTyCon  $ request_name req
                  ] ++ [resultType]


       shortFnDec = mkSimpleFun fnName
                    (map mkPVar shortArgs)
                    (HsDo shortBody)

       longFnDec = mkSimpleFun fnName
                   (map mkPVar ["c", "req"])
                   (HsDo longBody)

       shortArgs = "c" : fieldsToArgNames fields

       makeReceipt :: [HsStmt]
       makeReceipt | hasReply req = return $
                       mkGenerator (mkPVar "receipt")
                            (mkVar "newEmptyTMVarIO")
                   | otherwise = empty

       sendRequest :: [HsStmt]
       sendRequest | hasReply req = map HsQualifier
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

       shortBody :: [HsStmt]
       shortBody = concat
                   [ -- make the receipt if we need one
                     makeReceipt

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
                   , sendRequest
                   ]

       longBody :: [HsStmt]
       longBody = concat
                  [ makeReceipt

                  , [ getByteOrder
                    , mkLetStmt (mkPVar "chunk")
                      (applyManyExp
                       [mkVar "runPut"
                       ,mkVar "serialize" `HsApp` mkVar "bo" `HsApp` mkVar "req"
                       ])
                    ]                       

                  , sendRequest
                  ]

                         
       -- constructor plus args
       shortRequestExpr :: HsExp
       shortRequestExpr = 
           foldl1 HsApp $ constructor : map mkVar (fieldsToArgNames fields)

       -- TODO: share constructor name between
       -- generation condebases.
       constructor :: HsExp
       constructor = mkVar $ ("Mk"++) $ request_name req

       resultType | hasReply req = foldr1 HsTyApp $
                                    [mkTyCon "IO"
                                    ,mkTyCon "Receipt"
                                    ,replyType
                                    ]
                  | otherwise = foldr1 HsTyApp $
                                 [mkTyCon "IO"
                                 ,unit_tycon
                                 ]

       replyType = mkTyCon $ replyName req

applyManyExp [] = undefined
applyManyExp [x] = x
applyManyExp (x:xs) = HsApp x $ HsParen $ applyManyExp xs

fieldsToArgNames :: [StructElem] -> [String]
fieldsToArgNames = mapMaybe fieldToArgName

fieldToArgName :: StructElem -> Maybe String
fieldToArgName (List name _ _) = return name
fieldToArgName (SField name _) = return name
fieldToArgName (ValueParam _ mname _) = return $ valueParamName mname
fieldToArgName _ = empty

fieldsToTypes :: [StructElem] -> [HsType]
fieldsToTypes = mapMaybe fieldToType

fieldToType :: StructElem -> Maybe HsType
fieldToType (SField _ typ) = return . mkTyCon $ simpleType typ
fieldToType (List _ typ _) = return $ list_tycon `HsTyApp` mkTyCon (simpleType typ)
fieldToType (ValueParam typ _ _) = return $ mkTyCon "ValueParam" `HsTyApp` mkTyCon (simpleType typ)
fieldToType _ = empty

simpleType :: Type -> Name
simpleType QualType{} = error "simpleType: Unexpected compound type"
simpleType (UnQualType typ) = mapTyNames typ

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

requestFields :: RequestInfo -> [StructElem]
requestFields = filter go . request_elems
 where go List{} = True
       go SField{} = True
       go ValueParam{} = True
       go _ = False

hasReply :: RequestInfo -> Bool
hasReply = not . isNothing . request_reply

fnNameFromRequest :: RequestInfo -> String
fnNameFromRequest = ensureLower . request_name

replyName :: RequestInfo -> String
replyName req = assert (hasReply req) $
                request_name req ++ "Reply"
