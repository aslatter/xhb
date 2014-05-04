-- Generates the data types for a particular X module.
-- Also includes class instance declarations for those types
-- when appropriate.

{-# LANGUAGE
    FlexibleContexts  
 #-}

module Generate where

import Generate.Build
import Generate.Monad
import Generate.Facts

import Data.XCB

import HaskellCombinators

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
toHsModules xs = map (toHsModule transed) transed
  where transed =  standardTranslations xs

-- | Performs a single step of the 'toHsModules' conversion.
toHsModule :: [HXHeader] -> HXHeader -> HsModule
toHsModule = typesModule

typesModule :: [HXHeader] -> HXHeader -> HsModule
typesModule xs xhd =
    let newModule = newXhbTypesModule (formatName xhd)

        f = appMany
            [ decodeErrors xhd
            , decodeEvents xhd
            , processDeclarations xs xhd
            ]

  in f newModule

data ErrorDetail = ErrorDetail Name Int
data EventDetail = EventDetail Name Int

extractErrors :: GenXHeader a -> [ErrorDetail]
extractErrors = mapMaybe go . xheader_decls
    where go (XError name code _) = return $ ErrorDetail name code
          go _ = empty

extractEvents :: GenXHeader a -> [EventDetail]
extractEvents = mapMaybe go . xheader_decls
    where go (XEvent name code _ _) = return $ EventDetail name code
          go _ = empty

decodeErrors xhd = decodeErrorsOrEvents False xhd
decodeEvents xhd = decodeErrorsOrEvents True  xhd
 
-- write a function of type (OpCode -> Maybe (Get SomeError))
decodeErrorsOrEvents :: Bool -> HXHeader -> (HsModule -> HsModule)
decodeErrorsOrEvents event xhd =
    let errors = extractErrors xhd
        events = extractEvents xhd

    in appMany
           [ -- declare type
             addDecl $ mkTypeSig fnName [] (decodeFnType fnRetCon) 

           , -- declare cases on opcode
             addDecl $ if event then
              hsFunBind $ mapMaybe eventMatches events ++ [defaultMatch]
              else hsFunBind $ mapMaybe errorMatches errors ++ [defaultMatch]

           , -- export the function
             exportVar fnName
           ]

 where fnName | event = eventDecodeFn
              | otherwise = errorDecodeFn

       fnRetCon | event = "SomeEvent"
                | otherwise = "SomeError"

       wrapper | event = "toEvent"
               | otherwise = "toError"

       errorMatches :: ErrorDetail -> Maybe HsMatch
       errorMatches (ErrorDetail name code) 
           | code >= 0 = Just $ matches name code
           | otherwise = Nothing

       eventMatches :: EventDetail -> Maybe HsMatch
       eventMatches (EventDetail name code)
           | code >= 0 = Just $ matches name code
           | otherwise = Nothing

       matches name code =
           mkMatch fnName
             [ mkNumPat code
             ]
           (matchExp name)

       matchExp name = foldr1 (\x y -> x `hsApp` hsParen y)
              [ mkVar "return"
              , mkVar "liftM" `hsApp` mkVar wrapper
              , mkAsExp (mkVar "deserialize")
                        (mkTyCon "Get" `hsTyApp` mkTyCon name)
              ]

       defaultMatch = mkMatch fnName
             [hsPWildCard]
             (mkConExp "Nothing")


decodeFnType fnRetCon = foldr1 hsTyFun
         [ mkTyCon "Word8"
         , mkTyCon "Maybe" `hsTyApp`
           (mkTyCon "Get" `hsTyApp`
            mkTyCon fnRetCon)
         ]



-- do something per XDecl in the current module, in order
processDeclarations :: [HXHeader] -> HXHeader -> (HsModule -> HsModule)
processDeclarations xs xhd =
  appMany $ map (xDecl (xs, xhd)) $ xheader_decls xhd 

-- |Converts a declaration to a modification on a Haskell module
xDecl :: ([HXHeader], HXHeader) -> HXDecl -> (HsModule -> HsModule)
xDecl _ (XidType name)
    = appMany
      [ addDecl $
        simpleNewtype name "Xid"
        ["Eq","Ord","Show","Serialize","Deserialize","XidLike"]
      , exportTypeAbs name
      ]
xDecl x (XidUnion name _fields) =
            -- Pretend it's a declaration of an Xid Type
            xDecl x $ XidType name
xDecl _ (XStruct name fields) = appMany
  [ declareStruct name fields
  , addDecl $ declareSerStruct name fields
  , addDecl $ declareDeserStruct name fields
  , exportType name
  ]
xDecl _ (XTypeDef name typ) = appMany
  [ addDecl $ typeDecl name typ
  , exportTypeAbs name
  ]
xDecl (xs, xhd) (XImport name) = xImport xs xhd name
xDecl (_,xhd) (XRequest name opcode fields resp) = appMany
  [ declareStruct name fields
  , exportType name
  , addDecl $ declareSerRequest xhd name opcode fields
  , case resp of
      Nothing -> id -- empty
      Just rFields -> 
         let rName = replyName name
         in appMany
                [ declareStruct rName rFields
                , exportType rName
                , addDecl $ declareDeserReply rName rFields
                ]
  ]
xDecl _ (XEvent name opcode fields special) = appMany
  [ declareStruct name fields
  , addDecl $ declareEventInst name
  , addDecl $ declareDeserEvent name opcode fields special
  , exportType name
  ]
xDecl _ (XError name opcode fields) = appMany
  [ declareStruct name fields
  , addDecl $ declareErrorInst name
  , addDecl $ declareDeserError name fields
  , exportType name
  ]
xDecl _ dec@(XEnum nm elems') =
  let elems = cleanEnum . fillEnum $ elems'
      typ = verifyEnum dec elems
  in appMany
      [ addDecl $ declareEnumTycon nm elems
      , addDecl $ declareEnumInstance typ nm elems
      , exportType nm
      ]
xDecl _ (XUnion _ _) = id -- Unions are currently unhandled
xDecl _ x = error $ "Pattern match failed in \"xDecl\" with argument:\n"
            ++ (show $ toDoc x)

declareEventInst :: Name -> HsDecl
declareEventInst name = mkInstDecl
                        []
                        (mkUnQName $ packagePrefix ++ ".Shared.Event")
                        [mkTyCon name]
                        []

declareErrorInst :: Name -> HsDecl
declareErrorInst name = mkInstDecl
                        []
                        (mkUnQName $ packagePrefix ++ ".Shared.Error")
                        [mkTyCon name]
                        []


-- | For an X enum, declares an instance of 'SimpleEnum' of 'BitEnum'
-- as appropriate.
declareEnumInstance :: EnumType -> Name -> [EnumElem] -> HsDecl
declareEnumInstance _typ _name [] = error $ "declareEnumInstance: " ++
                                    "Enum has no elements"
declareEnumInstance ETypeValue name els =
      mkInstDecl
      []
      (mkUnQName "SimpleEnum")
      [mkTyCon name]
      [hsFunBind (map toVal els)
      ,hsFunBind (map fromVal els)
      ]
  where toVal (EnumElem nm (Just (Value n)))
            = mkConsMatch "toValue" (name ++ nm) (mkNumLit n)
        fromVal (EnumElem nm (Just (Value n)))
            = mkLitMatch "fromValue" (hsInt $ fromIntegral n) (hsCon (mkUnQName (name ++ nm)))

declareEnumInstance ETypeBit name els =
       mkInstDecl
       []
       (mkUnQName "BitEnum")
       [mkTyCon name]
       [hsFunBind (map toBit els)
       ,hsFunBind (map fromBit els)
       ]
   where toBit (EnumElem nm (Just (Bit n)))
             = mkConsMatch "toBit" (name ++ nm) (mkNumLit n)
         fromBit (EnumElem nm (Just (Bit n)))
             = mkLitMatch "fromBit" (hsInt (fromIntegral n)) $ hsCon $ mkUnQName $ name++nm

-- | For an X enum, declares a Haskell data type.
declareEnumTycon :: Name -> [EnumElem] -> HsDecl
declareEnumTycon name elems =
            mkDataDecl
            []
            name
            []
            (map (mkEnumCon name) elems)
            (mkUnQName <$> -- derving
              [ "Show"
              , "Eq"
              , "Ord"
              , "Enum"
              , "Typeable"
              ])
-- | For an element of an X enum, declares a clause in the Haskell data constructor
mkEnumCon :: Name -> EnumElem -> HsConDecl
mkEnumCon tyname (EnumElem name _) = mkCon (tyname ++ name) []


data EnumType = ETypeValue | ETypeBit | ETypeError String
 deriving (Show, Ord)

instance Eq EnumType where
    ETypeValue == ETypeValue = True
    ETypeBit == ETypeBit = True
    ETypeError{} == ETypeError{} = True
    _ == _ = False

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
verifyEnum :: Pretty a => GenXDecl a -> [EnumElem] -> EnumType
verifyEnum dec elems = case enumType elems of
        ETypeError str -> enumTypPanic str dec
        x -> x

-- | Returns the type of the enum elements.
-- An enum is either a 'Value' enum or a 'Bit' enum.
-- This is more strict than the xproto xml schema.
enumType :: [EnumElem] -> EnumType
enumType xs = case L.foldl' (flip go) Nothing xs of
                Nothing -> ETypeError "empty enum"
                Just x -> x
    where go x Nothing = return $ etyp x
          go _ jr@(Just ETypeError{}) = jr
          go x jr@(Just r) | etyp x == r = jr
          go _ _ = Just $ ETypeError "enum has mixed type"

          etyp (EnumElem _ (Just (Value {}))) = ETypeValue
          etyp (EnumElem _ (Just (Bit {})))   = ETypeBit
          etyp _                       = ETypeError "fatal error determining enum type"

enumTypPanic :: Pretty a => String -> GenXDecl a -> b
enumTypPanic str dec = error $
                   (("Error in enum, " ++ str ++ ":\n\n") ++) $
                    show $ toDoc dec

-- |If an enum doesn't have defined values fill them in
fillEnum :: [EnumElem] -> [EnumElem]
fillEnum = snd . L.mapAccumL go (Just 0)
    where
      go _ x@(EnumElem _ (Just (Value n))) = (Just (n+1), x)
      go (Just n) (EnumElem nm Nothing) = (Just (n+1), EnumElem nm (Just (Value n)))
      go acc x = (acc, x)

-- | If the X module declares that it imports another X module,
-- this function imports the corresponding Haskell module.
--
-- Conflicting declarations are imported qualified.
-- Non-conflicted declarations are imported normally.
xImport :: [HXHeader] -> HXHeader -> String -> (HsModule -> HsModule)
xImport xs cur str =
  let 
      impMod = fromJust $ findModule str xs -- bad error message
      shared_types = (L.intersect `on` declaredTypes) cur impMod

      vars_to_hide = [errorDecodeFn, eventDecodeFn]

      symbols_to_hide = shared_types ++ vars_to_hide

      impName = typesModuleName $ modName impMod
  in if null symbols_to_hide
   then addImport . mkImport $ impName
   else appMany
    [ addImport $ mkHidingImport impName symbols_to_hide
    , addImport . mkQualImport $ impName
    ]

-- |A list of all of the types defined by a module.
declaredTypes :: GenXHeader a -> [Name]
declaredTypes xhd =
    let decls = xheader_decls xhd

        tyName (XStruct name _) = return name
        tyName (XTypeDef name _) = return name
        tyName (XEvent name _ _ _) = return name
        tyName (XRequest name _ _ Nothing) = return name
        tyName (XRequest name _ _ _) = [name, replyName name]
        tyName (XidType name) = return name
        tyName (XidUnion name _) = return name
        tyName (XEnum name _) = return name
        tyName (XUnion name _) = return name
        tyName XImport{} = empty
        tyName (XError name _ _) = return name

    in concatMap tyName decls

hasErrorDecs :: GenXHeader a -> Bool
hasErrorDecs xhd =
    let decs = xheader_decls xhd
        
        p XError{} = True
        p _ = False

    in or $ map p decs 

hasEventDecs :: GenXHeader a -> Bool
hasEventDecs xhd =
    let decs = xheader_decls xhd

        p XEvent{} = True
        p _ = False

    in or $ map p decs

-- | An X type declaration.  Re-written to a Haskell type declaration.
-- Cross-module lookups of qualified types are handled here.
typeDecl :: String -> HsType -> HsDecl
typeDecl nm tp = 
  mkTypeDecl nm [] tp

-- | Given a type name and a list of X struct elements this declares
-- a Haskell data type.
declareStruct :: String
              -> [HStructElem]
              -> (HsModule -> HsModule)
declareStruct name fields =
     let selems = selemsToRec fields
         fExprFields = exprFields name fields

     in  appMany
          [ addDecl $ mkDataDecl
             []
             name
             []
             [mkRCon (conPrefix name) (selems)]
             (mkUnQName <$> ["Show","Typeable","Eq","Ord"])
          , fExprFields
          ]
    where selemsToRec :: [HStructElem] -> [(String,HsBangType)]
          selemsToRec xs = mapMaybe go xs

          go :: HStructElem -> Maybe (String, HsBangType)
          go elem = do
            nm <- fieldName elem
            hTp <- fieldType elem
            return (accessor nm name, hsUnBangedTy hTp)

fieldName :: Alternative a => HStructElem -> a Name
fieldName Pad{} = empty -- ignored
fieldName (List nm tp _ _) = pure nm
fieldName (SField nm tp _ _) = pure nm
fieldName (ValueParam _ mname _ _) = pure $ valueParamName mname
fieldName ExprField{} = empty -- deal with these spearate
fieldName selem = selemsToRecPanic selem

fieldType :: HStructElem -> Maybe HsType
fieldType Pad{} = empty
fieldType (List nm tp _ Nothing) = return $ hsTyApp list_tycon tp
fieldType (List nm _ _ (Just enum)) = return $ hsTyApp list_tycon enum
fieldType (SField nm tp Nothing Nothing) = return tp
fieldType (SField _ _ (Just enum) Nothing) = return enum
fieldType (SField _ _ _ (Just mask)) = return $ hsTyApp list_tycon mask
fieldType (ValueParam typ _ _ _) = return $ hsTyApp (mkTyCon "ValueParam") typ
fieldType ExprField{} = empty
fieldType selem = selemsToRecPanic selem

valueParamName :: Name -> Name
valueParamName mname = 
    let name = case nm of
                Nothing -> mname
                Just n -> reverse $ drop (n+1) $ rname
        rname = reverse mname
        nm = L.findIndex (== '_') rname
    in name

selemsToRecPanic :: HStructElem -> a
selemsToRecPanic x = error $
                     ("I dont know what to do with struct elem: " ++) $
                     show $ toDoc x


-- | Some identifiers clash with Haskell key-words.
-- This function renames those that do.
mapIdents :: String -> String
mapIdents "data" = "data_"
mapIdents "type" = "type_"
mapIdents "class" = "class_"
mapIdents x = x

-- |Generates an accesor-like function for each
-- expression-field in a struct.
exprFields :: Name -> [HStructElem] -> (HsModule -> HsModule)
exprFields name elems = appMany $ map go elems
                        
    where go :: HStructElem -> (HsModule -> HsModule)
          go (ExprField nm tp expr) =
            let funName = accessor nm name
                funTyp = hsTyFun (mkTyCon name) tp
                inVar = "x"
            
            in appMany $
             [
                -- Type signature
               addDecl $ mkTypeSig funName [] funTyp
            
               -- function body
             , addDecl $ mkSimpleFun funName [mkPVar inVar] $ 
                mkExpr (Just (inVar, name)) expr

              -- export
             , exportVar funName
             ]
          go _ = id


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
    = hsApp
      (mkVar $ accessor field name)
      (mkVar rec)
mkExpr Nothing (FieldRef field) = mkVar field
mkExpr rec (Op op lhs rhs) =
    let eLhs = mkExpr rec lhs
        eRhs = mkExpr rec rhs
    in hsParen $ hsApp (mkVar "fromIntegral") $ hsParen $ hsInfixApp eLhs (mkOp op) eRhs

mkOp :: Binop -> HsQOp
mkOp Add  = mkQOpSymbol "+"
mkOp Sub  = mkQOpSymbol "-"
mkOp Mult = mkQOpSymbol "*"
mkOp Div  = mkQOpSymbol "`div`"
mkOp And  = mkQOpSymbol ".&."
mkOp RShift = mkQOpIdent $ "shiftR"


----- Declaring serialize and deserialize instances

-- | Declare a instance of 'Deserialize' for an X struct
-- declaration.
declareDeserStruct :: Name -> [HStructElem] -> HsDecl
declareDeserStruct name fields =
    mkInstDecl
      []
      (mkUnQName "Deserialize")
      [tyCon $ mkUnQName name]
      [deserFunc]
   where
     deserFunc :: HsDecl
     deserFunc = mkSimpleFun
                  "deserialize"
                  []
                 (hsDo $ deserIns fields ++ [returnIt name fields])

-- | Declare and instance of 'Deserialize' for a reply to an X request.
declareDeserReply :: Name -> [HStructElem] -> HsDecl
declareDeserReply name fields =
    mkInstDecl
      []
      (mkUnQName "Deserialize")
      [tyCon $ mkUnQName name]
      [deserFunc]
   where
     deserFunc :: HsDecl
     deserFunc = mkSimpleFun
                 "deserialize"
                 []
                 (hsDo $ deserIns (doFields fields) ++ [declareLengthType, returnIt name fields])

     -- the same as the regular fields, except with more padding
     -- and the implicit length thrown in
     doFields (x1 : xs)
         = Pad 1 : x1 : Pad 2
           : SField "length" (mkTyCon "Word32") Nothing Nothing
           : xs

     declareLengthType :: HsStmt
     declareLengthType = hsLetStmt [mkPatBind hsPWildCard $ mkVar "isCard32" `hsApp` mkVar "length"]


declareDeserError :: Name -> [HStructElem] -> HsDecl
declareDeserError name elems =
    mkInstDecl
    []
    (mkUnQName "Deserialize")
    [mkTyCon name]
    [deserFunc]

   where deserFunc :: HsDecl
         deserFunc =
             mkSimpleFun
             "deserialize"
             []
             (hsDo $ deserIns (makeFields elems) ++ [returnIt name elems])

         makeFields :: [GenStructElem a] -> [GenStructElem a]
         makeFields xs =  Pad 4 : xs

declareDeserEvent :: Name -> Int -> [HStructElem] -> Maybe Bool -> HsDecl
declareDeserEvent name code elems special =

      mkInstDecl
         []
         (mkUnQName "Deserialize")
         [mkTyCon name]
         [mkSimpleFun "deserialize"
                []
                (hsDo $ deserIns (makeFields elems) ++ [returnIt name elems])
         ]

    where
      isKeymapNotify = case special of
                         Just True -> True
                         _ -> False

      makeFields :: [GenStructElem a] -> [GenStructElem a]
      makeFields xs | isKeymapNotify = Pad 1 : xs
                    | otherwise = case xs of
                           -- assumes the first field is one byte
                           (h:t) -> Pad 1 : h : Pad 2 : t
                           [] -> []

-- | Declare a statement in the 'do' block of the 'deserialize' function.
deserIns :: [HStructElem] -> [HsStmt]
deserIns fields = mapMaybe go fields
 where
     go (Pad n) = return $ hsQualifier $ mkVar "skip" `hsApp` mkNumLit n 
     go (List nm _typ Nothing _)
         = error "cannot deserialize list with no length"
     go (List nm _typ (Just exp) Nothing)
         = return $ mkGenerator (mkPVar $ mapIdents nm) $ hsAppMany
           [mkVar "deserializeList"
           ,hsParen $ mkVar "fromIntegral" `hsApp` mkExpr Nothing exp
           ]
     go (List nm baseType (Just exp) (Just enumType))
         = let deserType = mkTyCon "Get" `hsTyApp` (list_tycon `hsTyApp` baseType)
               deserExp  = hsParen $ (hsAppMany
                                      [mkVar "deserializeList"
                                      ,hsParen $ mkVar "fromIntegral" `hsApp` mkExpr Nothing exp
                                      ]) `mkAsExp` deserType
           in return $ mkGenerator (mkPVar $ mapIdents nm) $ hsAppMany
                  [mkVar "liftM" `hsApp` hsParen (mkVar "liftM" `hsApp` mkVar "fromValue")
                  ,deserExp
                  ]
     go (SField nm _typ Nothing Nothing)
         = return $ mkGenerator (mkPVar $ mapIdents nm) $
           mkVar "deserialize"
     go (SField nm typ (Just _enumTyp) Nothing) = return $
           let deserType = hsTyApp (mkTyCon "Get") typ
               deserExpr = hsParen $ mkVar "deserialize" `mkAsExp` deserType

           in mkGenerator (mkPVar $ mapIdents nm) $
                  (mkVar "liftM" `hsApp`
                   mkVar "fromValue") `hsApp`
                  deserExpr
       
     go (SField nm typ Nothing (Just maskTyp)) = return $
           let deserType = hsTyApp (mkTyCon "Get") typ
               deserExpr = hsParen $ mkVar "deserialize" `mkAsExp` deserType

           in mkGenerator (mkPVar $ mapIdents nm) $
                  (mkVar "liftM" `hsApp`
                   mkVar "fromMask") `hsApp`
                  deserExpr

     go ExprField{} = empty -- this is probbaly wrong, but I'm not sure where we need it
     go v@(ValueParam _ vname Nothing _) =
         let nm = mapIdents $ valueParamName vname
         in return $ mkGenerator (mkPVar nm) $
            mkVar "deserialize"
     go v@(ValueParam _ vname (Just pad) _) = 
         let nm = mapIdents $ valueParamName vname
         in return $ mkGenerator (mkPVar nm) $
            mkVar "deserializeValueParam" `hsApp` mkNumLit pad
     go n = error $ "Pattern match fail in deserIns.go with: " ++ show n

-- | Return and construct the deserialized value.
returnIt :: Name -> [HStructElem] -> HsStmt
returnIt name fields = hsQualifier $ mkVar "return" `hsApp` hsParen (cons name fields)

-- | Create and fill-in the constructor for the deserialized value.
cons :: Name -> [HStructElem] -> HsExp
cons name fields = hsAppMany $
       mkConExp (conPrefix name) : mapMaybe (liftM (mkVar . mapIdents) . fieldName) fields


-- | Declare an instance of 'Serialize' for an X struct.
declareSerStruct :: Name -> [HStructElem] -> HsDecl
declareSerStruct name fields =
    mkInstDecl
      []
      (mkUnQName "Serialize")
      [tyCon $ mkUnQName name]
      [serializeFunc,
       sizeFunc
      ]
  where
    sizeFunc :: HsDecl
    sizeFunc = mkSimpleFun "size"
                [mkPVar "x"]
                (L.foldl1' addExp $ map (toFieldSize name) fields)


    serializeFunc = mkSimpleFun "serialize"
          [mkPVar "x"]
          (hsDo $ map hsQualifier $ mapMaybe (serField name) fields)

-- | Declare an instance of "ExtensionRequest".
-- May not be called when generating code for a core
-- module.
declareExtRequest :: HXHeader -> Name -> Int -> [HStructElem] -> HsDecl
declareExtRequest xhd name opCode fields =
        let extName = (fromJust . xheader_xname) xhd
        in
         mkInstDecl
         []
         (mkUnQName "ExtensionRequest")
         [tyCon $ mkUnQName name]
         [extensionIdFunc extName
         ,serializeReqFunc
         ]
 where

   serActions = mapMaybe (serField name) fields
   sizeActions = map (toFieldSize name) fields

   extensionIdFunc :: Name -> HsDecl
   extensionIdFunc name =
       mkSimpleFun "extensionId"
        [hsPWildCard]
        (mkStringLit name)


   serializeReqFunc :: HsDecl
   serializeReqFunc = mkSimpleFun "serializeRequest"
        [mkPVar "x"
        ,mkPVar "extOpCode"
        ]
        (hsDo actions)

   actions :: [HsStmt]
   actions = (hsQualifier $ putIntExp $ mkVar "extOpCode")
           : (hsQualifier $ putIntExp $ mkNumLit opCode)
           : computeSize
           : hsQualifier putSize
           : map hsQualifier serActions
           ++ map hsQualifier [putPadding]

   computeSize :: HsStmt
   computeSize = mkLetStmt (mkPVar "size__") sizeCalc

   sizeCalc :: HsExp
   sizeCalc = L.foldl1' addExp $ mkNumLit 4 : sizeActions

   putSize = hsApp serializeExp $ hsParen $ mkAsExp sizeExp
             $ mkTyCon (mapTyNames "INT16")

   sizeExp = hsApp (mkVar "convertBytesToRequestSize") $
             mkVar "size__"

   putPadding = hsApp (mkVar "putSkip") $ hsParen $
                hsApp (mkVar "requiredPadding") $
                mkVar "size__"


putIntExp exp = mkVar "putWord8" `hsApp` exp
serializeExp = mkVar "serialize"

-- | Declare and instance of 'Serialize' for a request.
declareSerRequest :: HXHeader -> Name -> Int -> [HStructElem] -> HsDecl
declareSerRequest xhd name opCode fields = do
  if isExtension xhd
   then
      -- extension request case:
      -- declare instance of "ExtensionRequest"
      -- instead of "Serialize"
      declareExtRequest xhd name opCode fields
   else
      -- Core request
        mkInstDecl
        []
        (mkUnQName "Serialize")
        [tyCon $ mkUnQName name]
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
                 _ -> mkNumLit 3 : map (toFieldSize name) fields
    

    serializeFunc = mkSimpleFun "serialize"
           [mkPVar "x"]
           (hsDo $ map hsQualifier $ leadingActs ++ trailingActs)

    serActions = mapMaybe (serField name) fields

    leadingActs = [putIntExp $ mkNumLit opCode,firstAction serActions]
    trailingActs = (putSize : drop 1 serActions) ++ [putPadding]

    firstAction [] = mkVar "putSkip" `hsApp` mkNumLit 1
    firstAction (x:_) = x

    -- 'putSize', 'sizeExp' and 'putPadding' are similar to
    -- but not quite the same as the functions for extension
    -- reqeusts above.
    putSize = hsApp serializeExp $ hsParen $ mkAsExp sizeExp
              $ mkTyCon (mapTyNames "INT16")

    sizeExp = hsApp (mkVar "convertBytesToRequestSize") $
                hsParen $ mkVar "size" `hsApp` mkVar "x"

    putPadding = hsApp (mkVar "putSkip") $ hsParen $
                 hsApp (mkVar "requiredPadding") $  hsParen $
                 mkVar "size" `hsApp` mkVar "x"


-- | A statement in the "do" block for the 'serialize' function.
serField :: (Alternative m) => 
            Name -> HStructElem -> m HsExp
serField _ (Pad n) -- "putSkip n"
        = pure $ mkVar "putSkip" `hsApp` mkNumLit n
serField name (List lname _typ _expr Nothing) -- serializeList <list>
        = pure $ 
          hsApp (mkVar "serializeList") $ hsParen $
          accessField name lname
serField name (List lname typ _expr Just{}) = 
          let listType = hsTyApp list_tycon typ
              serExpr = (mkVar "map" `hsApp` mkVar "toValue")
                        `hsApp` hsParen (accessField name lname)

          in pure $ mkVar "serialize" `hsApp` hsParen
                     (serExpr `mkAsExp` listType)

serField name (SField fname _typ Nothing Nothing) -- serialize <field>
        = pure $ hsApp (mkVar "serialize") $ hsParen $
          accessField name fname
serField name (SField fname typ (Just _enumTyp) Nothing) =
          let serType = typ
              serExpr = mkVar "toValue" `hsApp` hsParen (accessField name fname)

          in pure $ mkVar "serialize" `hsApp` hsParen
                     (serExpr `mkAsExp` serType)

serField name (SField fname typ Nothing (Just _maskTyp)) =
          let serType = typ
              serExpr = mkVar "toMask" `hsApp` hsParen (accessField name fname)

          in pure $ mkVar "serialize" `hsApp` hsParen
                     (serExpr `mkAsExp` serType)
 
serField name (ExprField fname typ _exp)
    = serField name (SField fname typ Nothing Nothing)
serField name (ValueParam _ mname Nothing _) -- serialize <field>
        = pure $ hsApp (mkVar "serialize") $ hsParen $
          accessField name $ valueParamName mname
serField name (ValueParam typ mname (Just pad) lname)
        = pure $ hsApp (mkVar "serializeValueParam" `hsApp`
                          mkNumLit pad) $ hsParen $
          accessField name $ valueParamName mname

addExp :: HsExp -> HsExp -> HsExp
addExp = expBinop "+"

multExp = expBinop "*"

expBinop op lhs rhs = hsInfixApp lhs (mkQOpSymbol op) rhs

accessField name fieldName =
        mkVar (accessor fieldName name) `hsApp` mkVar "x"

sizeOfMember name fname = (mkVar "size" `hsApp`) $ hsParen $
                           accessField name fname

toFieldSize :: Name -> HStructElem -> HsExp
toFieldSize _ (Pad n) = mkNumLit n
toFieldSize name (List lname typ _expr Nothing) =
        (mkVar "sum" `hsApp`) $ hsParen $
        ((mkVar "map" `hsApp` mkVar "size") `hsApp`) $ hsParen $
        accessField name lname
toFieldSize name (List lname typ _expr Just{})
    = hsParen $ lhs `multExp` rhs
 where
   lhs = mkVar "length" `hsApp` accessField name lname
   rhs = mkVar "size" `hsApp` hsParen (mkVar "undefined" `mkAsExp` typ)
                                                

  -- length (accessorField name lname) * size (undefined :: Type)

toFieldSize name (SField fname _typ Nothing Nothing)
    = sizeOfMember name fname
toFieldSize name (SField fname typ _ _) = 
    mkVar "size" `hsApp` hsParen (mkVar "undefined" `mkAsExp` typ)

  -- size (undefined :: Type)


toFieldSize name (ExprField fname ftyp _)
    = toFieldSize name (SField fname ftyp Nothing Nothing)
toFieldSize name (ValueParam _ vname Nothing _) =
                sizeOfMember name . valueParamName $ vname
toFieldSize name (ValueParam typ mname (Just pad) lname) =
    let lhs = toFieldSize name (ValueParam typ mname Nothing lname)
        rhs = toFieldSize name (Pad pad)
    in hsParen $ lhs `addExp` rhs


-- |Defines a newtype declaration.
simpleNewtype :: String   -- typename
              -> String   -- wrapped type (unqualified)
              -> [String] -- derived typeclass instances
              -> HsDecl
simpleNewtype name typ cls =
    mkNewtype
     []
     name
     []
     (mkCon (conPrefix name) [hsUnBangedTy . tyCon $ mkUnQName typ])
     (map mkUnQName cls)

-- |Export the named type without exporting constructors.
-- Should be usable for type synonyms as well.
exportTypeAbs :: String -> (HsModule -> HsModule)
exportTypeAbs = addExport . mkExportAbs

-- |Export the named type/thing non-abstractly
exportType :: String -> (HsModule -> HsModule)
exportType = addExport . mkExportAll

-- |Export the named variable
exportVar :: String -> (HsModule -> HsModule)
exportVar = addExport . mkExportVar

-- |Like mapMaybe, but for any Alternative.
-- Never returns 'empty', instead returns 'pure []'
mapAlt :: Alternative f => (a -> f b) -> [a] -> f [b]
mapAlt f xs = go xs
 where go [] = pure []
       go (y:ys) = pure (:) <*> f y <*> go ys
               <|> go ys

embed :: Monad m => ReaderT r m a -> Reader r (m a)
embed m = reader $ \r -> runReaderT m r

