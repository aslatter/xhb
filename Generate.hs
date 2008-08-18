module Generate where

import Generate.Build
import Generate.Types

import Data.XCB

import HaskellCombinators
import Language.Haskell.Syntax

import Control.Monad.RW
import Control.Monad.Reader
import qualified Data.List as L
import Control.Applicative

import Data.Maybe
import Data.Function

toHsModule :: [XHeader] -> XHeader -> HsModule
toHsModule xs xhd =
    let rdata = ReaderData xhd xs
    in  runGen (modName xhd) rdata $ mapM_ xDecl (xheader_decls xhd)

toHsModules :: [XHeader] -> [HsModule]
toHsModules xs = map (toHsModule xs) xs


-- |Converts a declaration to a modification on a Haskell module
xDecl :: XDecl -> Gen
xDecl (XidType name) = do
  simpleNewtype name "Xid" ["Eq","Ord","Show","Serialize","Deserialize","XidLike"]
  exportTypeAbs name
xDecl (XidUnion name _fields) = xDecl $ XidType name  -- Pretend it's a declaration of an Xid Type  
xDecl (XStruct name fields) = do
  declareStruct name fields
  declareSerStruct name fields
  declareDeserStruct name fields
  exportType name
xDecl (XTypeDef name typ) = do
  typeDecl name typ
  exportTypeAbs name
xDecl (XImport name) = xImport name
xDecl (XRequest name opcode fields resp) = do
  declareStruct name fields
  exportType name
  declareSerRequest name opcode fields
  hasReply <- case resp of
    Nothing -> return False
    Just rFields -> do
              let rName = (name ++ "Reply")
              declareStruct rName rFields
              exportType rName
              declareDeserReply rName rFields
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
  let elems = cleanEnum . fillEnum $ elems'
      typ = verifyEnum dec elems
  declareEnumTycon nm elems
  declareEnumInstance typ nm elems
  exportType nm
xDecl (XUnion _ _) = return () -- Unions are currently unhandled
xDecl x = error $ "Pattern match failed in \"xDecl\" with argument:\n" ++ (show $ toDoc x)

declareEnumInstance :: EnumType -> Name -> [EnumElem] -> Gen
declareEnumInstance _typ _name [] = return ()
declareEnumInstance ETypeValue name els = buildDecl $
      mkInstDecl
      []
      (mkUnQName "SimpleEnum")
      [mkTyCon name]
      [HsFunBind (map toVal els)
      ,HsFunBind (map fromVal els)
      ]
  where toVal (EnumElem nm (Just (Value n)))
            = mkConsMatch "toValue" (name ++ nm) (mkNumLit n)
        fromVal (EnumElem nm (Just (Value n)))
            = mkLitMatch "fromValue" (HsInt $ fromIntegral n) (HsCon (mkUnQName (name ++ nm)))

declareEnumInstance ETypeBit name els = buildDecl $
       mkInstDecl
       []
       (mkUnQName "BitEnum")
       [mkTyCon name]
       [HsFunBind (map toBit els)
       ,HsFunBind (map fromBit els)
       ]
   where toBit (EnumElem nm (Just (Bit n)))
             = mkConsMatch "toBit" (name ++ nm) (mkNumLit n)
         fromBit (EnumElem nm (Just (Bit n)))
             = mkLitMatch "fromBit" (HsInt (fromIntegral n)) $ HsCon $ mkUnQName $ name++nm

declareEnumTycon :: Name -> [EnumElem] -> Gen
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

      bitElem (EnumElem _ (Just (Bit {}))) = True
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

          etyp (EnumElem _ (Just (Value {}))) = ETypeValue
          etyp (EnumElem _ (Just (Bit {})))   = ETypeBit
          etyp _                       = ETypeError

enumTypPanic :: XDecl -> a
enumTypPanic dec = error $
                   ("Error in enum:\n\n" ++) $
                   show $ toDoc dec

-- |If an enum doesn't have defined values fill them in
fillEnum :: [EnumElem] -> [EnumElem]
fillEnum xs@((EnumElem _ Nothing):_) = map f $ zip xs [0..]
    where f (EnumElem name _, n) = EnumElem name (Just (Value n))
fillEnum x = x

xImport :: String -> Gen
xImport str = do
  cur <- current
  impMod <- fromJust `liftM` oneModule str -- bad error message
  let shared_types = (L.intersect `on` declaredTypes) cur impMod
      impName = typesModulePrefix $ modName impMod
  if null shared_types
   then modifyModule . addImport . mkImport $ impName
   else do
    modifyModule . addImport $ mkHidingImport impName shared_types
    modifyModule . addImport . mkQualImport $ impName

-- |A list of all of the types defined by a module.
declaredTypes :: XHeader -> [Name]
declaredTypes xhd =
    let decls = xheader_decls xhd

        tyName (XStruct name _) = return name
        tyName (XTypeDef name _) = return name
        tyName (XEvent name _ _) = return name
        tyName (XRequest name _ _ Nothing) = return name
        tyName (XRequest name _ _ _) = [name, name ++ "Reply"]
        tyName (XidType name) = return name
        tyName (XidUnion name _) = return name
        tyName (XEnum name _) = return name
        tyName (XUnion name _) = return name
        tyName XImport{} = empty
        tyName (XError name _ _) = return name

    in concatMap tyName decls


typeDecl :: String -> Type -> Gen
typeDecl nm tp = do
  typName <- mapTyNames `liftM` fancyTypeName tp
  modifyModule . addDecl $
        mkTypeDecl nm [] (mkTyCon typName)

declareStruct :: String -> [StructElem] -> Gen
declareStruct name fields = do
         selems <- selemsToRec fields  
         buildDecl $
             mkDataDecl
             []
             name
             []
             [mkRCon (conPrefix name) (selems)]
             [mkUnQName "Show"]
         exprFields name fields
    where selemsToRec :: [StructElem] -> Generate [(String,HsBangType)]
          selemsToRec xs = do
            ys <- embed $ mapAlt go xs
            return $ fromJust ys -- mapAlt never returns Nothing

          go (Pad {})      = empty
          go (List nm tp _) = 
              do tyname <- fancyTypeName tp
                 return $
                  (accessor nm name, HsUnBangedTy $ listTyp tyname)
              where listTyp = HsTyApp list_tycon . mkTyCon . mapTyNames
          go (SField nm tp) = do
              tyname <- fancyTypeName tp
              return $ (accessor nm name, HsUnBangedTy $ mkTyCon tyname)
          go (ValueParam typ mname _lname) = do
            tyname <- fancyTypeName typ
            return $
             let nme = valueParamName mname
                 vTyp = HsTyApp (mkTyCon "ValueParam") (mkTyCon tyname)
             in (accessor nme name, HsUnBangedTy $ vTyp)
  
          go (ExprField{}) = empty -- deal with these separately
          go selem = selemsToRecPanic selem

valueParamName :: Name -> Name
valueParamName mname = 
    let name = case nm of
                Nothing -> mname
                Just n -> reverse $ drop (n+1) $ rname
        rname = reverse mname
        nm = L.findIndex (== '_') rname
    in name

selemsToRecPanic :: StructElem -> a
selemsToRecPanic x = error $
                     ("I dont know what to do with struct elem: " ++) $
                     show $ toDoc x

buildDecl :: HsDecl -> Gen
buildDecl = modifyModule . addDecl

mapTyNames :: String -> String
mapTyNames "char" = "CChar"
mapTyNames "void" = "Word8"
mapTyNames "float" = "CFloat"
mapTyNames "double" = "CDouble"
mapTyNames x = x

mapIdents :: String -> String
mapIdents "data" = "data_"
mapIdents "type" = "type_"
mapIdents "class" = "class_"
mapIdents x = x

exprFields :: Name -> [StructElem] -> Gen
exprFields name elems = mapM_ go elems
    where go (ExprField nm tp expr) = do
            tyname <- fancyTypeName tp
            let funName = accessor nm name
                retTyp = mkTyCon tyname
                funTyp = HsTyFun (mkTyCon name) retTyp
                inVar = "x"
            -- Type signature
            buildDecl $ mkTypeSig funName [] funTyp
            
            -- function body
            buildDecl $ mkSimpleFun funName [mkPVar inVar] $ 
                      mkExpr (Just (inVar, name)) expr

            -- export
            exportVar funName
          go _ = return ()


mkExpr :: Maybe (Name, Name) -> Expression -> HsExp
mkExpr _ (Value n) = mkNumLit n
mkExpr _ (Bit n) = mkNumLit $ 2^n
mkExpr (Just (rec, name)) (FieldRef field)
    = HsApp
      (mkVar $ accessor field name)
      (mkVar rec)
mkExpr Nothing (FieldRef field) = mkVar field
mkExpr rec (Op op lhs rhs) =
    let eLhs = mkExpr rec lhs
        eRhs = mkExpr rec rhs
    in HsParen $ HsApp (mkVar "fromIntegral") $ HsParen $ HsInfixApp eLhs (mkOp op) eRhs

mkOp :: Binop -> HsQOp
mkOp Add  = stringToQOpSymbol "+"
mkOp Sub  = stringToQOpSymbol "-"
mkOp Mult = stringToQOpSymbol "*"
mkOp Div  = stringToQOpSymbol "`div`"
mkOp And  = stringToQOpSymbol ".&."
mkOp RShift = HsQVarOp . UnQual . HsIdent $ "shiftR"

stringToQOpSymbol = HsQVarOp . UnQual . HsSymbol


-- |For the named newtype wrapper around an Xid,
-- declares an instance of FromXid.
-- Assumes the standard prefix is used for the
-- newtype data constructor.
instanceXid :: String -> Gen
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


----- Declaring serialize and deserialize instances

declareDeserStruct :: Name -> [StructElem] -> Gen
declareDeserStruct name fields = modifyModule . addDecl $
    mkInstDecl
      []
      (mkUnQName "Deserialize")
      [HsTyCon $ mkUnQName name]
      [deserFunc]
   where
     deserFunc :: HsDecl
     deserFunc = mkSimpleFun
                  "deserialize"
                  [mkPVar "bo"
                  ]
                 (HsDo $ deserIns fields ++ [returnIt name fields])

declareDeserReply :: Name -> [StructElem] -> Gen
declareDeserReply name fields = modifyModule . addDecl $
    mkInstDecl
      []
      (mkUnQName "Deserialize")
      [HsTyCon $ mkUnQName name]
      [deserFunc]
   where
     deserFunc :: HsDecl
     deserFunc = mkSimpleFun
                 "deserialize"
                 [mkPVar "bo"]
                 (HsDo $ deserIns (doFields fields) ++ [declareLengthType, returnIt name fields])

     -- the same as the regular fields, except with more padding
     -- and the implicit length thrown in
     doFields (x1 : xs) = Pad 1 : x1 : Pad 2 : SField "length" (UnQualType "CARD32") : xs

     declareLengthType :: HsStmt
     declareLengthType = HsLetStmt [mkPatBind HsPWildCard $ mkVar "isCard32" `HsApp` mkVar "length"]

deserIns :: [StructElem] -> [HsStmt]
deserIns fields = mapMaybe go fields
 where
     go (Pad n) = return $ HsQualifier $ mkVar "skip" `HsApp` mkNumLit n 
     go (List nm typ Nothing) = error "cannot deserialize list with no length"
     go (List nm typ (Just exp))
         = return $ mkGenerator (mkPVar $ mapIdents nm) $ hsAppMany
           [mkVar "deserializeList"
           ,mkVar "bo"
           ,HsParen $ mkVar "fromIntegral" `HsApp` mkExpr Nothing exp
           ]

     go (SField nm typ) = return $ mkGenerator (mkPVar $ mapIdents nm) $
             mkVar "deserialize" `HsApp` mkVar "bo"
     go ExprField{} = empty
     go v@(ValueParam _ vname _) = let nm = mapIdents $ valueParamName vname
                         in return $ mkGenerator (mkPVar nm) $
                            mkVar "deserialize" `HsApp` mkVar "bo"
     go n = error $ "Pattern match fail in deserIns.go with: " ++ show n


returnIt :: Name -> [StructElem] -> HsStmt
returnIt name fields = HsQualifier $ mkVar "return" `HsApp` HsParen (cons name fields)

cons :: Name -> [StructElem] -> HsExp
cons name fields = hsAppMany $
       mkConExp (conPrefix name) : mapMaybe (liftM (mkVar . mapIdents) . fieldName) fields

fieldName :: StructElem -> Maybe Name
fieldName Pad{} = empty
fieldName (List name _ _) = Just name
fieldName (SField name _) = Just name
fieldName ExprField{} = empty -- has a name, but we don't want it
fieldName (ValueParam _ name _) = return $ valueParamName name


declareSerStruct :: Name -> [StructElem] -> Gen
declareSerStruct name fields = modifyModule . addDecl $
    mkInstDecl
      []
      (mkUnQName "Serialize")
      [HsTyCon $ mkUnQName name]
      [serializeFunc,
       sizeFunc
      ]
  where
    sizeFunc :: HsDecl
    sizeFunc = mkSimpleFun "size"
                [mkPVar "x"]
                (L.foldl1' addExp $ mapMaybe (toFieldSize name) fields)


    serializeFunc = mkSimpleFun "serialize"
          [mkPVar "bo"
          ,mkPVar "x"]
          (HsDo $ map HsQualifier $ mapMaybe (serField name) fields)

-- | Returns 'True' if code is being generated for an extension
isExtension :: Generate Bool
isExtension = do
  xhd <- current
  return $ not $ isNothing $ xheader_xname xhd

declareSerRequest :: Name -> Int -> [StructElem] -> Gen
declareSerRequest name opCode fields = do
  ext <- isExtension
  if ext then return () else
      modifyModule . addDecl $
        mkInstDecl
        []
        (mkUnQName "Serialize")
        [HsTyCon $ mkUnQName name]
        [serializeFunc,
         sizeFunc
        ]
  where
    sizeFunc :: HsDecl
    sizeFunc = mkSimpleFun "size"
                [mkPVar "x"]
                (L.foldl1' addExp sizeExps)

    sizeExps :: [HsExp]
    sizeExps = case serActions of
                 [] -> [mkNumLit 4]
                 _ -> mkNumLit 3 : mapMaybe (toFieldSize name) fields

    serializeFunc = mkSimpleFun "serialize"
           [mkPVar "bo"
           ,mkPVar "x"]
           (HsDo $ map HsQualifier $ leadingActs ++ trailingActs)

    serActions = mapMaybe (serField name) fields
    leadingActs = [putIntExp opCode,firstAction serActions]
    trailingActs = (putSize : drop 1 serActions) ++ [putPadding]

    firstAction [] = mkVar "putSkip" `HsApp` mkNumLit 1
    firstAction (x:_) = x

    putIntExp n = mkVar "putInt8" `HsApp` mkNumLit n

    putSize = HsApp serializeExp $ HsParen $ mkAsExp sizeExp $ mkTyCon "INT16"

    serializeExp = mkVar "serialize" `HsApp` mkVar "bo"

    sizeExp = HsApp (mkVar "convertBytesToRequestSize") $
                HsParen $ mkVar "size" `HsApp` mkVar "x"

    putPadding = HsApp (mkVar "putSkip") $ HsParen $
                 HsApp (mkVar "requiredPadding") $  HsParen $
                 mkVar "size" `HsApp` mkVar "x"

serField :: Name -> StructElem -> Maybe HsExp
serField _ (Pad n) -- "putSkip n"
        = return $ mkVar "putSkip" `HsApp` mkNumLit n
serField name (List lname _typ _expr) -- serializeList bo <list>
        = return $ 
          HsApp (mkVar "serializeList" `HsApp` mkVar "bo") $ HsParen $
          accessField name lname
serField name (SField fname _typ) -- serialize bo <field>
        = return $ HsApp (mkVar "serialize" `HsApp` mkVar "bo") $ HsParen $
          accessField name fname
serField _ ExprField{}  = Nothing
serField name (ValueParam _ mname _) -- serialize bo <field>
        = return $ HsApp (mkVar "serialize" `HsApp` mkVar "bo") $ HsParen $
          accessField name $ valueParamName mname


addExp :: HsExp -> HsExp -> HsExp
addExp = expBinop "+"

expBinop op lhs rhs = HsInfixApp lhs (HsQVarOp . UnQual $ HsSymbol op) rhs

accessField name fieldName =
        mkVar (accessor fieldName name) `HsApp` mkVar "x"

    -- sizeOfType typ = (mkVar "size" `HsApp`) $ HsParen $
    --                 mkVar "undefined" `mkAsExp` HsTyCon (mkUnQName typ)

sizeOfMember name fname = (mkVar "size" `HsApp`) $ HsParen $
                           accessField name fname

toFieldSize :: Name -> StructElem -> Maybe HsExp
toFieldSize _ (Pad n) = return $ mkNumLit n
toFieldSize name (List lname typ _expr) = return $
        (mkVar "sum" `HsApp`) $ HsParen $
        ((mkVar "map" `HsApp` mkVar "size") `HsApp`) $ HsParen $
        accessField name lname
toFieldSize name (SField fname _typ) = return $ sizeOfMember name fname
toFieldSize _ ExprField{} = Nothing
toFieldSize name (ValueParam _ vname _) = return $
                sizeOfMember name . valueParamName $ vname

-- |Defines a newtype declaration.
simpleNewtype :: String   -- typename
              -> String   -- wrapped type (unqualified)
              -> [String] -- derived typeclass instances
              -> Gen
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
exportTypeAbs :: String -> Gen
exportTypeAbs = modifyModule . addExport . mkExportAbs

-- |Export the named type/thing non-abstractly
exportType :: String -> Gen
exportType = modifyModule . addExport . mkExportAll

-- |Export the named variable
exportVar :: String -> Gen
exportVar = modifyModule . addExport . HsEVar . mkUnQName

-- |Like mapMaybe, but for any Alternative.
-- Never returns 'empty', instead returns 'pure []'
mapAlt :: Alternative f => (a -> f b) -> [a] -> f [b]
mapAlt f xs = go xs
 where go [] = pure []
       go (y:ys) = pure (:) <*> f y <*> go ys
               <|> go ys
