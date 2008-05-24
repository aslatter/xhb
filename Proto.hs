module Proto where

import Types
import HaskellCombinators
import BuildData

import Language.Haskell.Syntax
import Language.Haskell.Pretty

import Control.Monad.Writer
import Data.Char
import qualified Data.Map as M

conPrefix = ("Mk" ++)
modulePrefix =  ("XCB.Gen." ++)

accessor :: String -> String -> String
accessor field typ = field ++ "_" ++ typ

----- Builder monad details
type Builder = Writer BuildData
type Build = Builder ()

-- |Execute the modification of the module under build.
modifyModule :: (HsModule -> HsModule) -> Build
modifyModule = tell . buildHsModule

-- |Call this after building an event type.
-- It logs the event opcode, and eventually adds it to the module-level
-- event variant type.
logEvent :: EventName -> Int -> Build
logEvent name code = tell $ buildEvent name code

-- |Call this after building a request type.
-- It logs the request opcode, and eventually adds it to the module-level
-- request variant type.
logRequest :: RequestName -> Int -> Bool -> Build
logRequest name code hasReply = tell $ buildRequest name code hasReply

-- |Call this after building a request type.
-- It logs the request opcode, and eventually adds it to the module-level
-- error variant type.
logError :: ErrorName -> Int -> Build
logError name code = tell $ buildError name code

runBuilder :: String -> Builder a -> (a, HsModule)
runBuilder name bldr =
    let (x,bdata) = runWriter bldr
        newModule = fModExtras $ mkModule $ modulePrefix name
        BuildResult mod mapEvent mapReq mapErr = applyBuildData bdata newModule

        -- function which adds the module sum-types for events, errors and requests
        fModExtras = mkEventType name mapEvent . mkRequestType name mapReq . mkErrorType name mapErr
    in (x, mod)

-- takes all of the results of running 'logEvent' and turns it into to two things:
--   * a sum-type of all the events in this module
--   * a function mapping that sum-type to it's event opcode
--
-- hopefully we can auto-generate srialization/desrialization here as well
mkEventType :: String -- Module name
            -> M.Map EventName Int
            -> HsModule
            -> HsModule
mkEventType name namesMap = 

    let tyName = name ++ "Event"

        -- Function mapping a module event to its event code
        toCodeFunName = "toEventCode"
        toCodeFnTyp = mkTypeSig toCodeFunName [] (HsTyFun (mkTyCon tyName) (mkTyCon "Int"))
        toCodeFnDec = HsFunBind $ map go $ M.toList namesMap
            where go (typName, code) = 
                      let cons = typName -- the super-type uses the base typenames as constructors
                          result = mkNumLit $ fromIntegral code
                      in mkConsMatch toCodeFunName cons result

        -- Type of any event in this module
        eventTypDec = mkDataDecl [] tyName [] dataCons []
            where dataCons :: [HsConDecl]
                  dataCons = map f $ M.keys namesMap

                  f :: String -> HsConDecl
                  f eventTyName = mkCon eventTyName [HsUnBangedTy $ mkTyCon eventTyName]

        eventExports :: [HsExportSpec]
        eventExports = [mkExportAll tyName
                       ,mkExportAbs toCodeFunName
                       ]
                        

        eventDeser  = undefined -- action in Get, deserialization
        eventSer    = undefined -- action in Put, serialization

    in appMany (map addExport eventExports)
           . addDecl eventTypDec
           . addDecl toCodeFnTyp
           . addDecl toCodeFnDec

appMany :: [a -> a] -> (a -> a)
appMany = foldr (.) id

{-
   'eventDeser' and 'eventSer' need more information on how the event opcode
   is sent down the wire for X."
-}

mkRequestType :: String -- Module name
              -> M.Map RequestName (Int,Bool)
              -> HsModule
              -> HsModule
mkRequestType name map = id

mkErrorType :: String -- Module name
            -> M.Map ErrorName Int
            -> HsModule
            -> HsModule
mkErrorType name map = id


runBuild :: String -> Build -> HsModule
runBuild name bld= snd $ runBuilder name bld

prettyBuild :: String -> Build -> String
prettyBuild name bld = prettyPrint $ runBuild name bld
-----

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
  hasReply <- case resp of
    Nothing -> return False
    Just rFields -> do
              let rName = (name ++ "Reply")
              declareStruct rName rFields
              exportType rName
              -- declare instances of serialize/deserialize?
              return True
  logRequest name opcode hasReply

xImport :: String -> Build
xImport = modifyModule . addImport . mkImport . modulePrefix . ensureUpper

ensureUpper :: String -> String
ensureUpper [] = []
ensureUpper (x:xs) = (toUpper x) : xs

typeDecl :: String -> String -> Build
typeDecl nm tp = modifyModule . addDecl $
  mkTypeDecl nm [] (mkTyCon tp)

declareStruct :: String -> [StructElem] -> Build
declareStruct name fields = modifyModule . addDecl $
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
instanceXid tyname = modifyModule . addDecl $
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
    modifyModule $
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
exportTypeAbs = modifyModule . addExport . mkExportAbs

-- |Export the named type/thing non-abstractly
exportType :: String -> Build
exportType = modifyModule . addExport . mkExportAll


-- Random utiltiy functions

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

twist :: (Ord a, Ord b) => M.Map a b -> M.Map b a
twist = M.fromList . map swap . M.toList

