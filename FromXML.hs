{- {-# LANGUAGE PatternSignatures, ScopedTypeVariables #-} -}

{-

What we need if a way to allow modules to copy events, errors, etc.
across module boundaries.

The idea is that XML parsing takes place in a Writer/Reader monad.

The Reader is fed the results of the Writer.

Hopefuly black-holes do not ensue.

-}


-- something like:

module FromXML where

import Types
import Pretty

import Text.XML.Light

import Data.List as List
import Data.Maybe
import Data.Monoid

import Control.Monad
import Control.Monad.Reader


fromFiles :: [FilePath] -> IO [XHeader]
fromFiles xs = do
  strings <- sequence $ map readFile xs
  return $ fromStrings strings


fromStrings :: [String] -> [XHeader]
fromStrings xs =
   let rs = mapMaybeParse fromString xs
       Just headers = runReaderT rs headers
   in headers 

-- The 'Parse' monad.  Provides the name of the
-- current module, and a list of all of the modules.
type Parse = ReaderT ([XHeader],Name) Maybe

-- operations in the 'Parse' monad

localName :: Parse Name
localName = snd `liftM` ask

allModules :: Parse [XHeader]
allModules = fst `liftM` ask

first :: Maybe a -> Maybe a -> Maybe a
first a b = getFirst $ First a `mappend` First b

lookupThingy :: ([XDecl] -> Maybe a)
             -> (Maybe Name)
             -> Parse (Maybe a)
lookupThingy f Nothing = do
  lname <- localName
  liftM2 first (lookupThingy f $ Just lname)
               (lookupThingy f $ Just "xproto") -- implicit xproto import
lookupThingy f (Just mname) = do
  xs <- allModules
  return $ do
    XHeader _ _ decls <- findXHeader mname xs
    f decls

lookupEvent :: Maybe Name -> Name -> Parse (Maybe EventDetails)
lookupEvent mname evname = flip lookupThingy mname $ \decls ->
                 findEvent evname decls

lookupError :: Maybe Name -> Name -> Parse (Maybe ErrorDetails)
lookupError mname ername = flip lookupThingy mname $ \decls ->
                 findError ername decls

findXHeader :: Name -> [XHeader] -> Maybe XHeader
findXHeader name = List.find $ \ (XHeader name' _ _) -> name == name'

findError :: Name -> [XDecl] -> Maybe ErrorDetails
findError pname xs =
      case List.find f xs of
        Nothing -> Nothing
        Just (XError name code elems) -> Just $ ErrorDetails name code elems
    where  f (XError name _ _) | name == pname = True
           f _ = False 
                                       
findEvent :: Name -> [XDecl] -> Maybe EventDetails
findEvent pname xs = 
      case List.find f xs of
        Nothing -> Nothing
        Just (XEvent name code elems) -> Just $ EventDetails name code elems
   where f (XEvent name _ _) | name == pname = True
         f _ = False 

data EventDetails = EventDetails Name Int [StructElem]
data ErrorDetails = ErrorDetails Name Int [StructElem]

---

fromString :: String -> ReaderT [XHeader] Maybe XHeader
fromString str = do
  el@(Element qname ats cnt _) <- lift $ parseXMLDoc str
  guard $ el `named` "xcb"
  modName <- el `attr` "header"
  let exinfo = extractExInfo el
  decls <- withReaderT (\r -> (r,modName)) $ extractDecls cnt
  return $ XHeader modName exinfo decls


extractExInfo :: Element -> Maybe ExInfo
extractExInfo el = do
  xn <- el `attr` "extension-xname"
  n  <- el `attr` "extension-name"
  v1 <- el `attr` "major-version"
  v2 <- el `attr` "minor-version"
  return $ ExInfo n xn (v1,v2)

extractDecls :: [Content] -> Parse [XDecl]
extractDecls = mapMaybeParse declFromElem . onlyElems

-- |Elements which evaluate to failure are dropped
mapMaybeParse :: (a -> ReaderT r Maybe b)
              -> [a]
              -> ReaderT r Maybe [b]
mapMaybeParse f xs = ReaderT $ \r -> 
        let ms = map ((\m -> runReaderT m r) . f) xs
        in  Just $ mapMaybe id ms
                         

declFromElem :: Element -> Parse XDecl
declFromElem elem 
    | elem `named` "request" = xrequest elem
    | elem `named` "event"   = xevent elem
    | elem `named` "eventcopy" = xevcopy elem
    | elem `named` "error" = xerror elem
    | elem `named` "errorcopy" = xercopy elem
    | elem `named` "struct" = xstruct elem
    | elem `named` "union" = xunion elem
    | elem `named` "xidtype" = xidtype elem
    | elem `named` "xidunion" = xidunion elem
    | elem `named` "typedef" = xtypedef elem
    | elem `named` "enum" = xenum elem
    | otherwise = fail "no parse"

xenum :: Element -> Parse XDecl
xenum elem = do
  nm <- elem `attr` "name"
  fields <- mapMaybeParse enumField $ elChildren elem
  guard $ not $ null fields
  return $ XEnum nm fields

enumField :: Element -> Parse EnumElem
enumField elem = do
  guard $ elem `named` "item"
  name <- elem `attr` "name"
  expr <- firstChild elem >>= expression
  return $ EnumElem name expr

xrequest :: Element -> Parse XDecl
xrequest elem = do
  nm <- elem `attr` "name"
  code <- elem `attr` "opcode" >>= readM
  fields <- mapMaybeParse structField $ elChildren elem
  let reply = getReply elem
  guard $ not (null fields) || not (isNothing reply)
  return $ XRequest nm code fields reply

getReply :: Element -> Maybe XReply
getReply elem = do
  childElem <- unqual "reply" `findChild` elem
  let fields = mapMaybe structField $ elChildren childElem
  guard $ not $ null fields
  return fields

xevent :: Element -> Parse XDecl
xevent elem = do
  name <- elem `attr` "name"
  number <- elem `attr` "number" >>= readM
  fields <- mapMaybeParse structField $ elChildren elem
  guard $ not $ null fields
  return $ XEvent name number fields

xevcopy :: Element -> Parse XDecl
xevcopy elem = do
  name <- elem `attr` "name"
  number <- elem `attr` "number" >>= readM
  ref <- elem `attr` "ref"
  -- do we have a qualified ref?
  let (mname,evname) = splitRef ref
  details <- lookupEvent mname evname 
  return $ XEvent name number $ case details of
               Nothing -> error $ "Unresolved event: " ++ show mname ++ " " ++ ref
               Just (EventDetails _ _ x) -> x

splitRef :: Name -> (Maybe Name, Name)
splitRef ref = case split ':' ref of
                 (x,"") -> (Nothing, x)
                 (a, b) -> (Just a, b)

-- |Neither returned string contains the first occurance of the
-- supplied Char.
split :: Char -> String -> (String, String)
split c xs = go xs
    where go [] = ([],[])
          go (x:xs) | x == c = ([],xs)
                    | otherwise = 
                        let (lefts, rights) = go xs
                        in (x:lefts,rights)
                 

xerror :: Element -> Parse XDecl
xerror elem = do
  name <- elem `attr` "name"
  number <- elem `attr` "number" >>= readM
  fields <- mapMaybeParse structField $ elChildren elem
  guard $ not $ null fields
  return $ XError name number fields


xercopy :: Element -> Parse XDecl
xercopy elem = do
  name <- elem `attr` "name"
  number <- elem `attr` "number" >>= readM
  ref <- elem `attr` "ref"
  let (mname, ername) = splitRef ref
  details <- lookupError mname ername
  return $ XError name number $ case details of
               Nothing -> error $ "Unresolved error: " ++ show mname ++ " " ++ ref
               Just (ErrorDetails _ _ x) -> x

xstruct :: Element -> Parse XDecl
xstruct elem = do
  name <- elem `attr` "name"
  fields <- mapMaybeParse structField $ elChildren elem
  guard $ not $ null fields
  return $ XStruct name fields

xunion :: Element -> Parse XDecl
xunion elem = do
  name <- elem `attr` "name"
  fields <- mapMaybeParse structField $ elChildren elem
  guard $ not $ null fields
  return $ XUnion name fields

xidtype :: Element -> Parse XDecl
xidtype elem = liftM XidType $ elem `attr` "name"

xidunion :: Element -> Parse XDecl
xidunion elem = do
  name <- elem `attr` "name"
  let types = mapMaybe xidUnionElem $ elChildren elem
  guard $ not $ null types
  return $ XidUnion name types

xidUnionElem :: Element -> Maybe XidUnionElem
xidUnionElem elem = do
  guard $ elem `named` "type"
  return $ XidUnionElem $ strContent elem

xtypedef :: Element -> Parse XDecl
xtypedef elem = do
  oldname <- elem `attr` "oldname"
  newname <- elem `attr` "newname"
  return $ XTypeDef newname oldname


structField :: MonadPlus m => Element -> m StructElem
structField elem
    | elem `named` "field" = do
        typ <- elem `attr` "type"
        name <- elem `attr` "name"
        return $ SField name typ

    | elem `named` "pad" = do
        bytes <- elem `attr` "bytes" >>= readM
        return $ Pad bytes

    | elem `named` "list" = do
        typ <- elem `attr` "type"
        name <- elem `attr` "name"
        expr <- firstChild elem >>= expression
        return $ List name typ expr

    | elem `named` "valueparam" = do
        mask_typ <- elem `attr` "value-mask-type"
        mask_name <- elem `attr` "value-mask-name"
        list_name <- elem `attr` "value-list-name"
        return $ ValueParam mask_typ mask_name list_name

    | elem `named` "exprfield" = do
        typ <- elem `attr` "type"
        name <- elem `attr` "name"
        expr <- firstChild elem >>= expression
        return $ ExprField name typ expr

    | elem `named` "reply" = fail "" -- handled separate

    | otherwise = let name = elName elem
                  in error $ "I don't know what to do with structelem "
 ++ show name

expression :: MonadPlus m => Element -> m Expression
expression elem | elem `named` "fieldref"
                    = return $ FieldRef $ strContent elem
                | elem `named` "value"
                    = Value `liftM` readM (strContent elem)
                | elem `named` "bit"
                    = Bit `liftM` do
                        n <- readM (strContent elem)
                        guard $ n >= 0
                        return n
                | elem `named` "op" = do
                    binop <- elem `attr` "op" >>= toBinop
                    [exprLhs,exprRhs] <- mapM expression $ elChildren elem
                    return $ Op binop exprLhs exprRhs

toBinop :: MonadPlus m => String -> m Binop
toBinop "+"  = return Add
toBinop "-"  = return Sub
toBinop "*"  = return Mult
toBinop "/"  = return Div
toBinop "&"  = return And
toBinop "&amp;" = return And
toBinop ">>" = return RShift
toBinop _ = fail ""


{-  
-- Post processing function.
-- Eliminates 'XEventCopy' and 'XErrorCopy' declarations
postProcess :: [XDecl] -> [XDecl]
postProcess decls = 
    let (decls', (eventsM,errorsM)) = flip runState mempty $ mapM go decls

        recordEvent event@(XEvent name _ _)
            = modify $ \(evs,ers) -> (M.insert name event evs,ers)

        recordError err@(XError name _ _)
            = modify $ \(evs,ers) -> (evs,M.insert name err ers)

        go event@(XEvent {}) = recordEvent event >> return event
        go   err@(XError {}) = recordError err   >> return err

        go (XEventCopy name code ref) 
            = return $ XEvent name code $ case M.lookup ref eventsM of
                    Nothing -> error $ "Invaild reference to event: " ++ ref
                    Just (XEvent _ _ fields) -> fields

        go (XErrorCopy name code ref)
            = return $ XError name code $ case M.lookup ref errorsM of
                    Nothing -> error $ "Invalid reference to error: " ++ ref
                    Just (XError _ _ fields) -> fields

        go x = return x
    in decls'
-}             
  

----
----
-- Utility functions
----
----

firstChild :: Monad m => Element -> m Element
firstChild = listToM . elChildren

listToM :: Monad m => [a] -> m a
listToM [] = fail "empty list"
listToM (x:xs) = return x

named :: Element -> String -> Bool
named (Element qname _ _ _) name | qname == unqual name = True
named _ _ = False

attr :: Monad m => Element -> String -> m String
(Element _ xs _ _) `attr` name = case List.find p xs of
      Just (Attr _ res) -> return res
      _ -> fail "not found"
    where p (Attr qname _) | qname == unqual name = True
          p _ = False

-- adapted from Network.CGI.Protocol
readM :: (Monad m, Read a) => String -> m a
readM = liftM fst . listToM . reads

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads