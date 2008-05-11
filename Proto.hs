module Proto where

import Types
import HaskellCombinators

import Language.Haskell.Syntax
import Language.Haskell.Pretty

import Control.Monad.State
import Data.Char

newtype Builder a = MkBuilder a
type Build = Builder ()

conPrefix = ("Mk" ++)
modulePrefix =  ("XCB.Gen." ++)

accessor :: String -> String -> String
accessor field typ = field ++ "_" ++ typ

runBuilder :: String -> Builder a -> (a, HsModule)
runBuilder name = undefined

runBuild :: String -> Build -> HsModule
runBuild = snd . runBuilder

prettyBuild :: String -> Build -> String
prettyBuild name bld = prettyPrint $ runBuild name bld


xDecl :: XDecl -> Build
xDecl (XidType name) = do
  simpleNewtype name "Xid" ["Eq","Ord","Show","Serialize","Deserialize","XidLike"]
  exportTypeAbs name
xDecl (XidUnion name _fields) = xDecl $ XidType name  -- Pretend it's a declaration of an Xid Type  
xDecl (XStruct name fields) = do
  declareStruct name fields
  exportType name
xDecl (XTypeDef name typ) = do
  typeDecl name typ
  exportTypeAbs name
xDecl (XImport name) = xImport name
xDecl (XRequest name opcode fields resp) = do
  declareStruct name fields
  exportType name
  -- declare instances or serialize/deserialize?
  case resp of
    Nothing -> return ()
    Just rFields -> do
              let rName = (name ++ "Reply")
              declareStruct rName rFields
              exportType rName
              -- declare instances of serialize/deserialize?

xImport :: String -> Build
xImport = modify . addImport . mkImport . modulePrefix . ensureUpper

ensureUpper :: String -> String
ensureUpper [] = []
ensureUpper (x:xs) = (toUpper x) : xs

typeDecl :: String -> String -> Build
typeDecl nm tp = modify . addDecl $
  mkTypeDecl nm [] (mkTyCon tp)

declareStruct :: String -> [StructElem] -> Build
declareStruct name fields = modify . addDecl $
  mkDataDecl
   []
   name
   []
   [mkRCon (conPrefix name) (selemsToRec fields)]
   []
 where selemsToRec :: [StructElem] -> [(String,HsBangType)]
       selemsToRec [] = []
       selemsToRec (Pad {} :xs)      = selemsToRec xs
       selemsToRec (ListSize {} :xs) = selemsToRec xs
       selemsToRec (List nm tp _:xs) = -- Needs to be updated for operators
           (accessor nm name, HsUnBangedTy $ HsTyApp list_tycon (mkTyCon tp))
           : selemsToRec xs
       selemsToRec (SField nm tp:xs) =
           (accessor nm name, HsUnBangedTy $ mkTyCon tp)
           : selemsToRec xs


-- |For the named newtype wrapper around an Xid,
-- declares an instance of FromXid.
-- Assumes the standard prefix is used for the
-- newtype data constructor.
instanceXid :: String -> Build
instanceXid tyname = modify . addDecl $
   mkInstDecl
    []
    (mkUnQName "XidLike")
    [HsTyCon $ mkUnQName tyname]
    [mkSimpleFun "fromXid" [] (HsCon $ mkUnQName (conPrefix tyname))
    ,mkSimpleFun
      "toXid"
      [HsPApp (mkUnQName (conPrefix tyname)) [mkPVar "a"]]
      (HsVar $ mkUnQName "a")
    ]

-- |Defines a newtype declaration.
simpleNewtype :: String   -- typename
              -> String   -- wrapped type (unqualified)
              -> [String] -- derived typeclass instances
              -> Build
simpleNewtype name typ cls =
    modify $
    addDecl $
    mkNewtype
     []
     name
     []
     (mkCon (conPrefix name) [HsUnBangedTy . HsTyCon $ mkUnQName typ])
     (map (UnQual . HsIdent) cls)

-- |Export the named type without exporting constructors.
-- Should be usable for type synonyms as well.
exportTypeAbs :: String -> Build
exportTypeAbs = modify . addExport . HsEAbs . mkUnQName

-- |Export the named type/thing non-abstractly
exportType :: String -> Build
exportType = modify . addExport . HsEThingAll . mkUnQName
