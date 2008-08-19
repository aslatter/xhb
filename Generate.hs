-- Generates the data types for a particular X module.
-- Also includes class instance declarations for those types
-- when appropriate.

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

-- | Converts X modules to Haskell modules declaring the
-- appropriate data types.
--
-- All modules which are involved in cross-module
-- qualified types must be converted at the same time.
--
-- All modules which are involved in importing each
-- other must be converted at the same time.
toHsModules :: [XHeader] -> [HsModule]
toHsModules xs = map (toHsModule xs) xs

-- | Performs a single step of the 'toHsModules' conversion.
toHsModule :: [XHeader] -> XHeader -> HsModule
toHsModule xs xhd =
    let rdata = ReaderData xhd xs
    in  runGen (modName xhd) rdata $ mapM_ xDecl (xheader_decls xhd)


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

-- | For an X enum, declares an instance of 'SimpleEnum' of 'BitEnum'
-- as appropriate.
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

-- | For an X enum, declares a Haskell data type.
declareEnumTycon :: Name -> [EnumElem] -> Gen
declareEnumTycon name elems = modifyModule . addDecl $ 
            mkDataDecl
            []
            name
            []
            (map (mkEnumCon name) elems)
            [] -- derving

-- | For an element of an X enum, declares a clause in the Haskell data constructor
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

-- | Throws an error if we're not prepared to generate code
-- for an X enum.
--
-- In particular, we disallow enums with both regular numbers
-- and bit-field numbers.
verifyEnum :: XDecl -> [EnumElem] -> EnumType
verifyEnum dec elems = case enumType elems of
        ETypeError -> enumTypPanic dec
        x -> x

-- | Returns the type of the enum elements.
-- An enum is either a 'Value' enum or a 'Bit' enum.
-- This is more strict than the xproto xml schema.
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

-- | If the X module declares that it imports another X module,
-- this function imports the corresponding Haskell module.
--
-- Conflicting declarations are imported qualified.
-- Non-conflicted declarations are imported normally.
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


-- | An X type declaration.  Re-written to a Haskell type declaration.
-- Cross-module lookups of qualified types are handled here.
typeDecl :: String -> Type -> Gen
typeDecl nm tp = do
  typName <- mapTyNames `liftM` fancyTypeName tp
  modifyModule . addDecl $
        mkTypeDecl nm [] (mkTyCon typName)

-- | Given a type name and a list of X struct elements this declares
-- a Haskell data type.
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

-- | Adds a declaration to the module currently being generated.
buildDecl :: HsDecl -> Gen
buildDecl = modifyModule . addDecl

-- | Some types in the X modules are given using C types.
-- This function maps those strings to the appropriate Haskell
-- types.
mapTyNames :: String -> String
mapTyNames "char" = "CChar"
mapTyNames "void" = "Word8"
mapTyNames "float" = "CFloat"
mapTyNames "double" = "CDouble"
mapTyNames x = x

-- | Some identifiers clash with Haskell key-words.
-- This function renames those that do.
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


-- | Convert an 'Expression' to a Haskell expression.
--
-- The first argument is non-nothing when used in the context of
-- writing a deserialization function.  The first element of the pair
-- is the name of the variable which is being deserialized and the second
-- element is the name of the type being deserialized.
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

-- | Declare a instance of 'Deserialize' for an X struct
-- declaration.
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

-- | Declare and instance of 'Deserialize' for a reply to an X request.
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

-- | Declare a statement in the 'do' block of the 'deserialize' function.
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

-- | Return and construct the deserialized value.
returnIt :: Name -> [StructElem] -> HsStmt
returnIt name fields = HsQualifier $ mkVar "return" `HsApp` HsParen (cons name fields)

-- | Create and fill-in the constructor for the deserialized value.
cons :: Name -> [StructElem] -> HsExp
cons name fields = hsAppMany $
       mkConExp (conPrefix name) : mapMaybe (liftM (mkVar . mapIdents) . fieldName) fields

fieldName :: StructElem -> Maybe Name
fieldName Pad{} = empty
fieldName (List name _ _) = Just name
fieldName (SField name _) = Just name
fieldName ExprField{} = empty -- has a name, but we don't want it
fieldName (ValueParam _ name _) = return $ valueParamName name


-- | Declare an instance of 'Serialize' for an X struct.
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


-- | Declare an instance of "ExtensionRequest".
-- May not be called when generating code for a core
-- module.
declareExtRequest name opCode fields = do
        extName <- (fromJust . xheader_xname) `liftM` current
        modifyModule . addDecl $
         mkInstDecl
         []
         (mkUnQName "ExtensionRequest")
         [HsTyCon $ mkUnQName name]
         [extensionIdFunc extName
         ,serializeReqFunc
         ]
 where

   serActions = mapMaybe (serField name) fields
   sizeActions = mapMaybe (toFieldSize name) fields

   extensionIdFunc :: Name -> HsDecl
   extensionIdFunc name =
       mkSimpleFun "extensionId"
        [HsPWildCard]
        (HsLit . HsString $  name)


   serializeReqFunc :: HsDecl
   serializeReqFunc = mkSimpleFun "serializeRequest"
        [mkPVar "x"
        ,mkPVar "extOpCode"
        ,mkPVar "bo"
        ]
        (HsDo actions)

   actions :: [HsStmt]
   actions = (HsQualifier $ putIntExp $ mkVar "extOpCode")
           : (HsQualifier $ putIntExp $ mkNumLit opCode)
           : computeSize
           : HsQualifier putSize
           : map HsQualifier serActions
           ++ map HsQualifier [putPadding]

   computeSize :: HsStmt
   computeSize = mkLetStmt (mkPVar "size__") sizeCalc

   sizeCalc :: HsExp
   sizeCalc = L.foldl1' addExp $ mkNumLit 4 : sizeActions

   putSize = HsApp serializeExp $ HsParen $ mkAsExp sizeExp $ mkTyCon "INT16"

   sizeExp = HsApp (mkVar "convertBytesToRequestSize") $
             mkVar "size__"

   putPadding = HsApp (mkVar "putSkip") $ HsParen $
                HsApp (mkVar "requiredPadding") $
                mkVar "size__"


putIntExp exp = mkVar "putWord8" `HsApp` exp
serializeExp = mkVar "serialize" `HsApp` mkVar "bo"

-- | Declare and instance of 'Serialize' for a request.
declareSerRequest :: Name -> Int -> [StructElem] -> Gen
declareSerRequest name opCode fields = do
  ext <- isExtension
  if ext
   then
      -- extension request case:
      -- declare instance of "ExtensionRequest"
      -- instead of "Serialize"
      declareExtRequest name opCode fields
   else
      -- Core request
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

    leadingActs = [putIntExp $ mkNumLit opCode,firstAction serActions]
    trailingActs = (putSize : drop 1 serActions) ++ [putPadding]

    firstAction [] = mkVar "putSkip" `HsApp` mkNumLit 1
    firstAction (x:_) = x

    -- 'putSize', 'sizeExp' and 'putPadding' are similar to
    -- but not quite the same as the functions for extension
    -- reqeusts above.
    putSize = HsApp serializeExp $ HsParen $ mkAsExp sizeExp $ mkTyCon "INT16"

    sizeExp = HsApp (mkVar "convertBytesToRequestSize") $
                HsParen $ mkVar "size" `HsApp` mkVar "x"

    putPadding = HsApp (mkVar "putSkip") $ HsParen $
                 HsApp (mkVar "requiredPadding") $  HsParen $
                 mkVar "size" `HsApp` mkVar "x"

-- | A statement in the "do" block for the 'serialize' function.
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
