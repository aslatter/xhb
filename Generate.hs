module Generate where

import Generate.Build
import Generate.Types

import Data.XCB
 -- import XCB.Utils

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


-- |Coverts a declartion to a mdification on a haskell module
xDecl :: XDecl -> Gen
xDecl (XidType name) = do
  simpleNewtype name "Xid" ["Eq","Ord","Show","Serialize","Deserialize","XidLike"]
  exportTypeAbs name
xDecl (XidUnion name _fields) = xDecl $ XidType name  -- Pretend it's a declaration of an Xid Type  
xDecl (XStruct name fields) = do
  declareStruct name fields
  declareSerStruct name fields
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
      impName = modulePrefix $ modName impMod
  if null shared_types
   then modifyModule . addImport . mkImport $ impName
   else do
    modifyModule . addImport $ mkHidingImport impName shared_types
    modifyModule . addImport . mkQualImport $ impName

-- |A list of all of the type defined by a modue.
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
             []
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
  
          go (ExprField{}) = empty -- deal wth these separate
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

exprFields :: Name -> [StructElem] -> Gen
exprFields name elems = mapM_ go elems
    where go (ExprField nm tp expr) = do
            tyname <- fancyTypeName tp
            let funName = accessor nm name
                retTyp = mkTyCon tyname
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
            exportVar funName
          go _ = return ()


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
    sizeFunc = mkSimpleFun
                "size"
                [mkPVar "x"]
                (L.foldl1' add $ mapMaybe toFieldSize fields)

    add :: HsExp -> HsExp -> HsExp
    add = expBinop "+"

    mult :: HsExp -> HsExp -> HsExp
    mult = expBinop "*"

    expBinop op lhs rhs = HsInfixApp lhs (HsQVarOp . UnQual $ HsSymbol op) rhs

    accessField fieldName =
        mkVar (accessor fieldName name) `HsApp` mkVar "x"

    sizeOfType typ = (mkVar "size" `HsApp`) $ HsParen $
                     mkVar "undefined" `mkAsExp` HsTyCon (mkUnQName typ)

    sizeOfMember fname = (mkVar "size" `HsApp`) $ HsParen $
                           accessField fname

    toFieldSize :: StructElem -> Maybe HsExp
    toFieldSize (Pad n) = return $ mkNumLit n
    toFieldSize (List lname typ _expr) = return $
         (mkVar "sum" `HsApp`) $ HsParen $
         ((mkVar "map" `HsApp` mkVar "size") `HsApp`) $ HsParen $
         accessField lname
    toFieldSize (SField fname _typ) = return $ sizeOfMember fname
    toFieldSize (ExprField _ _ _) = Nothing
    toFieldSize (ValueParam _ name _) = return $
          sizeOfMember . valueParamName $ name


    serializeFunc = mkSimpleFun "serialize"
          [mkPVar "bo"
          ,mkPVar "x"]
          (HsDo $ map HsQualifier $ mapMaybe serField fields)

    serField :: StructElem -> Maybe HsExp
    serField (Pad n) -- "putSkip n"
        = return $ mkVar "putSkip" `HsApp` mkNumLit n
    serField (List name _typ _expr) -- serializeList bo <list>
        = return $ 
          HsApp (mkVar "serializeList" `HsApp` mkVar "bo") $ HsParen $
          accessField name
    serField (SField fname _typ) -- serialize bo <field>
        = return $ HsApp (mkVar "serialize" `HsApp` mkVar "bo") $ HsParen $
          accessField fname
    serField ExprField{}  = Nothing
    serField (ValueParam _ mname _) -- serialize bo <field>
        = return $ HsApp (mkVar "serialize" `HsApp` mkVar "bo") $ HsParen $
          accessField $ valueParamName mname

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
