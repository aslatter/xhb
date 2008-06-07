module Proto where

import Types
import HaskellCombinators
import BuildData
import Pretty

import Language.Haskell.Syntax
import Language.Haskell.Pretty

import Control.Monad.Writer
import Data.Char
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe

conPrefix = ("Mk" ++)
modulePrefix =  ("XHB.Gen." ++)

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
runBuilder nm bldr =
    let (x,bdata) = runWriter bldr
        newModule = fModExtras $ newXhbModule name
        BuildResult mod mapEvent mapReq mapErr = applyBuildData bdata newModule
        name = ensureUpper nm

        -- function which adds the module sum-types for events, errors and requests
        fModExtras = mkEventType name mapEvent . mkRequestType name mapReq . mkErrorType name mapErr
    in (x, mod)


-- Create a new XHB generated module
newXhbModule :: String -> HsModule
newXhbModule = addStandardImports . mkModule . modulePrefix . ensureUpper
    where addStandardImports = addImport $ mkImport "XHB.Shared"

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

    in appMany (map addExport eventExports)
           . addDecl toCodeFnDec
           . addDecl toCodeFnTyp
           . addDecl eventTypDec

mkRequestType :: String -- Module name
              -> M.Map RequestName (Int,Bool)
              -> HsModule
              -> HsModule
mkRequestType name namesMap =
    let tyName = name ++ "Request"

        toCodeFunName = "toRequestCode"
        toCodeFnTyp = mkTypeSig toCodeFunName [] (HsTyFun (mkTyCon tyName) (mkTyCon "Int"))
        toCodeFnDec = HsFunBind $ map go $ M.toList namesMap
            where go (typName, (code,_)) =
                      let cons = typName
                          result = mkNumLit $ fromIntegral code
                      in mkConsMatch toCodeFunName cons result

        hasRepFunName = "hasReply"
        hasRepFnTyp = mkTypeSig hasRepFunName [] (HsTyFun (mkTyCon tyName) (mkTyCon "Bool"))
        hasRepFnDec = HsFunBind $ map go $ M.toList namesMap
            where go (typName, (_,hasReply)) =
                      let cons = typName
                          result = HsCon $ mkUnQName $ show hasReply
                      in mkConsMatch hasRepFunName cons result


        requestTypDec = mkDataDecl [] tyName [] dataCons []
            where dataCons :: [HsConDecl]
                  dataCons = map f $ M.keys namesMap

                  f :: String -> HsConDecl
                  f eventTyName = mkCon eventTyName [HsUnBangedTy $ mkTyCon eventTyName]

        requestExports :: [HsExportSpec]
        requestExports = [mkExportAll tyName
                         ,mkExportAbs toCodeFunName
                         ,mkExportAbs hasRepFunName
                         ]

    in appMany (map addExport requestExports)
       . appMany (map addDecl [requestTypDec
                              ,toCodeFnTyp
                              ,toCodeFnDec
                              ,hasRepFnTyp
                              ,hasRepFnDec
                              ]) 

mkErrorType :: String -- Module name
            -> M.Map ErrorName Int
            -> HsModule
            -> HsModule
mkErrorType name namesMap =
    let tyName = name ++ "Error"

        toCodeFunName = "toErrorCode"
        toCodeFnTyp = mkTypeSig toCodeFunName [] (HsTyFun (mkTyCon tyName) (mkTyCon "Int"))
        toCodeFnDec = HsFunBind $ map go $ M.toList namesMap
            where go (typName, code) = 
                      let cons = typName
                          result = mkNumLit $ fromIntegral code
                      in mkConsMatch toCodeFunName cons result

        errorTypDec = mkDataDecl [] tyName [] dataCons []
            where dataCons :: [HsConDecl]
                  dataCons = map f $ M.keys namesMap

                  f :: String -> HsConDecl
                  f eventTyName = mkCon eventTyName [HsUnBangedTy $ mkTyCon eventTyName]

        errorExports :: [HsExportSpec]
        errorExports = [mkExportAll tyName
                       ,mkExportAbs toCodeFunName
                       ]

    in appMany (map addExport errorExports)
           . addDecl toCodeFnDec
           . addDecl toCodeFnTyp
           . addDecl errorTypDec

appMany :: [a -> a] -> (a -> a)
appMany = foldr (flip (.)) id


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
xDecl (XEvent name opcode fields) = do
  declareStruct name fields
  exportType name
  logEvent name opcode
xDecl (XError name opcode fields) = do
  declareStruct name fields
  exportType name
  logError name opcode
xDecl dec@(XEnum nm elems') = do
  let elems = cleanEnum elems'
      typ = verifyEnum dec elems
  declareEnumTycon nm elems
  declareEnumInstance typ nm elems
  -- export something
xDecl (XUnion _ _) = return () -- Unions are currently unhandled
xDecl x = error $ "Pattern match failed in \"xDecl\" with argument:\n" ++ (show $ toDoc x)

declareEnumInstance :: EnumType -> Name -> [EnumElem] -> Build
declareEnumInstance _typ _name [] = return ()
declareEnumInstance ETypeValue name els = buildDecl $
      mkInstDecl
      []
      (mkUnQName "SimpleEnum")
      [mkTyCon name]
      [HsFunBind (map toVal els)
      ,HsFunBind (map fromVal els)
      ]
  where toVal (EnumElem nm (Value n))
            = mkConsMatch "toValue" (name ++ nm) (mkNumLit n)
        fromVal (EnumElem nm (Value n))
            = mkLitMatch "fromValue" (HsInt $ fromIntegral n) (HsCon (mkUnQName (name ++ nm)))

declareEnumInstance ETypeBit name els = buildDecl $
       mkInstDecl
       []
       (mkUnQName "BitEnum")
       [mkTyCon name]
       [HsFunBind (map toBit els)
       ,HsFunBind (map fromBit els)
       ]
   where toBit (EnumElem nm (Bit n))
             = mkConsMatch "toBit" (name ++ nm) (mkNumLit n)
         fromBit (EnumElem nm (Bit n))
             = mkLitMatch "fromBit" (HsInt (fromIntegral n)) $ HsCon $ mkUnQName $ name++nm

declareEnumTycon :: Name -> [EnumElem] -> Build
declareEnumTycon name elems = modifyModule . addDecl $ 
            mkDataDecl
            []
            name
            []
            (map (mkEnumCon name) elems)
            [] -- derving

mkEnumCon :: Name -> EnumElem -> HsConDecl
mkEnumCon tyname (EnumElem name _) = mkCon (tyname ++ name) []


data EnumType = ETypeValue | ETypeBit | ETypeError
 deriving (Eq, Show, Enum, Bounded, Ord)

cleanEnum :: [EnumElem] -> [EnumElem]
cleanEnum xs =
  let containsBits = not . null $ justBits

      justBits = filter bitElem xs

      bitElem (EnumElem _ (Bit {})) = True
      bitElem _ = False

  in if containsBits
      then justBits
      else xs

verifyEnum :: XDecl -> [EnumElem] -> EnumType
verifyEnum dec elems = case enumType elems of
        ETypeError -> enumTypPanic dec
        x -> x

-- spine strict. element strict under normal conditions
enumType :: [EnumElem] -> EnumType
enumType xs = case L.foldl' (flip go) Nothing xs of
                Nothing -> ETypeError
                Just x -> x
    where go x Nothing = return $ etyp x
          go _ jr@(Just ETypeError) = jr
          go x jr@(Just r) | etyp x == r = jr
          go _ _ = Just ETypeError

          etyp (EnumElem _ (Value {})) = ETypeValue
          etyp (EnumElem _ (Bit {}))   = ETypeBit
          etyp _                       = ETypeError

enumTypPanic :: XDecl -> a
enumTypPanic dec = error $
                   ("Error in enum:\n\n" ++) $
                   show $ toDoc dec

xImport :: String -> Build
xImport = modifyModule . addImport . mkImport . modulePrefix . ensureUpper

ensureUpper :: String -> String
ensureUpper [] = []
ensureUpper (x:xs) = (toUpper x) : xs

typeDecl :: String -> String -> Build
typeDecl nm tp = modifyModule . addDecl $
  mkTypeDecl nm [] (mkTyCon tp)

declareStruct :: String -> [StructElem] -> Build
declareStruct name fields = do
         buildDecl $
             mkDataDecl
             []
             name
             []
             [mkRCon (conPrefix name) (selemsToRec fields)]
             []
         exprFields name fields
    where selemsToRec :: [StructElem] -> [(String,HsBangType)]
          selemsToRec xs = mapMaybe go xs

          go (Pad {})      = Nothing
          go (List nm tp _) = return $ -- Needs to be updated for operators
                       (accessor nm name, HsUnBangedTy $ listTyp tp)
              where listTyp = HsTyApp list_tycon . mkTyCon . mapTyNames
          go (SField nm tp) = return $
                       (accessor nm name, HsUnBangedTy $ mkTyCon tp)
          go (ValueParam typ mname _lname) = return $
           let nme = case nm of
                        Nothing -> mname
                        Just n -> reverse $ drop (n+1) $ rname
               rname = reverse mname
               nm = L.findIndex (== '_') rname
               vTyp = HsTyApp (mkTyCon "ValueParam") (mkTyCon typ)
           in (accessor nme name, HsUnBangedTy $ vTyp)
  
          -- go _ = Nothing -- cheater for testing
          go (ExprField{}) = Nothing -- deal wth these separate
          go selem = selemsToRecPanic selem


selemsToRecPanic :: StructElem -> a
selemsToRecPanic x = error $
                     ("I dont know what to do with struct elem: " ++) $
                     show $ toDoc x

buildDecl :: HsDecl -> Build
buildDecl = modifyModule . addDecl

mapTyNames :: String -> String
mapTyNames "char" = "CChar"
mapTyNames x = x

exprFields :: Name -> [StructElem] -> Build
exprFields name elems = mapM_ go elems
    where go (ExprField nm tp expr) = do
            let funName = accessor nm name
                retTyp = mkTyCon tp
                funTyp = HsTyFun (mkTyCon name) retTyp
            -- Type signature
            buildDecl $ mkTypeSig funName [] funTyp
            
            let inVar = "x"
                
                mkExpr :: Expression -> HsExp
                mkExpr (Value n) = mkNumLit n
                mkExpr (Bit n) = mkNumLit $ 2^n
                mkExpr (FieldRef field)
                    = HsApp
                      (mkVar $ accessor field name)
                      (mkVar inVar)
                mkExpr (Op op lhs rhs) =
                    let eLhs = mkExpr lhs
                        eRhs = mkExpr rhs
                    in HsParen $ HsInfixApp eLhs (mkOp op) eRhs

                mkOp :: Binop -> HsQOp
                mkOp Add  = HsQVarOp . UnQual . HsSymbol $ "+"
                mkOp Sub  = HsQVarOp . UnQual . HsSymbol $ "-"
                mkOp Mult = HsQVarOp . UnQual . HsSymbol $ "*"
                mkOp Div  = HsQVarOp . UnQual . HsSymbol $ "/"
                mkOp And  = HsQVarOp . UnQual . HsSymbol $ ".&."
                mkOp RShift = HsQVarOp . UnQual . HsIdent $ "shiftR"

            -- function body
            buildDecl $ mkSimpleFun funName [mkPVar inVar] (mkExpr expr)

            -- export
            exportType funName
          go _ = return ()


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

