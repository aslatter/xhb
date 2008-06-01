module FromXML(fromString) where

-- Using HaXml 1.19
-- Maybe I could do some CPP magic to support both
-- HaXml 1.19 and 1.13.

import Text.XML.HaXml.Types
 hiding (Name)
import Text.XML.HaXml.Parse

import qualified Data.List as L
import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import Control.Monad.State
import Data.Monoid

import Types

fromString :: String -> String -> XHeader
fromString fname str = 
    let Document _ _ elem _ = xmlParse fname str
    in fromElem elem

-- |Given an XML element, returns an 'XHeader'.
-- The element must be named "xcb".
fromElem :: Element i -> XHeader
fromElem el|el `named` "xcb" =
         let Just modName = attr "header" el
             exinfo = extractExInfo el
             decs = extractDecls el
         in XHeader modName exinfo decs
fromElem _  = error "No parse in Types.fromElem"

-- |Given an element an the LHS of an attribute assignment,
-- return the RHS as 'Just rhs'.  XML attributes are more
-- complex than simple '(String, String)' pairs.  This complexity
-- is ignored.
attr :: String -> Element i -> Maybe String
attr nm (Elem _ ats _) = do
  atv <- snd `liftM` L.find ((== nm) . fst) ats
  return $ show atv

-- |Returns true if the passed in element's name
-- is the passed in string.
named :: Element i -> String -> Bool
named (Elem nm _ _) nm'| nm == nm' = True
named _ _ = False

-- |Given an element, attempts to interpret it as a
-- description of an 'ExInfo' value.
extractExInfo :: Element i -> Maybe ExInfo
extractExInfo el = do
  xn <- attr "extension-xname" el
  n  <- attr "extension-name"  el
  v1 <- attr "major-version"   el
  v2 <- attr "minor-version"   el
  return $ ExInfo n xn (v1,v2)
-- NOTE:  The above function isn't quite right.  The individual
-- components of the ExInfo value should be optional, not the whole
-- thing.

-- |Given an element, attempts to interpret the element's
-- contents as decribing a series of 'XDecl' values.
extractDecls :: Element i -> [XDecl]
extractDecls (Elem _ _ cnt) = postProcess $ mapMaybe xdecls cnt
 where
   xdecls (CElem elem _)
       | elem `named` "request" = xrequest elem
       | elem `named` "event"   = xevent elem
       | elem `named` "eventcopy" = xevcopy elem
       | elem `named` "error" = xerror elem
       | elem `named` "errorcopy" = xercopy elem
       | elem `named` "struct" = xstruct elem
       | elem `named` "union" = xunion elem -- ?!?
       | elem `named` "xidtype" = xidtype elem
       | elem `named` "xidunion" = xidunion elem
       | elem `named` "typedef" = xtypedef elem
       | elem `named` "enum" = xenum elem
   xdecls _ = Nothing

xenum :: Element i -> Maybe XDecl
xenum elem = do
  nm <- "name" `attr` elem
  let fields = mapMaybe go cnt
      Elem _ _ cnt = elem
      go (CElem el _) = enumField el
      go _ = Nothing
  guard $ not $ null fields
  return $ XEnum nm fields

enumField :: Element i -> Maybe EnumElem
enumField elem = do
  guard $ elem `named` "item"
  name <- "name" `attr` elem
  celem <- firstChildElem elem
  expr <- expression celem
  return $ EnumElem name expr

xrequest :: Element i -> Maybe XDecl
xrequest elem = do
  nm <- "name" `attr` elem
  code_string <- "opcode" `attr` elem
  code <- maybeRead code_string
  let fields = mapMaybe go cnt
      Elem _ _ cnt = elem
      go (CElem el _) = structField el
      go _ = Nothing
  let reply = getReply elem
  guard $ not (null fields) || not (isNothing reply)
  return $ XRequest nm code fields reply

getReply :: Element i -> Maybe XReply
getReply elem = do
  cnt <- elem `contentOfChild` "reply"
  let fields = mapMaybe go cnt
      go (CElem el _) = structField el
      go _ = Nothing
  guard $ not $ null fields
  return fields

contentOfChild :: Element i -> String -> Maybe [Content i]
contentOfChild (Elem _ _ cnt) nm = listToMaybe $ mapMaybe go cnt
 where go (CElem (Elem nm' _ cout) _)|nm' == nm = return cout
       go _ = Nothing 

xevent :: Element i -> Maybe XDecl
xevent elem = do
  nm <- "name" `attr` elem
  number_string <- "number" `attr` elem
  number <- maybeRead number_string
  let fields = mapMaybe go cnt
      Elem _ _ cnt = elem
      go (CElem el _) = structField el
      go _ = Nothing
  guard $ not $ null fields 
  return $ XEvent nm number fields
        

xevcopy :: Element i -> Maybe XDecl
xevcopy elem = do
  nm <- "name" `attr` elem
  number_string <- "number" `attr` elem
  number <- maybeRead number_string
  ref <- "ref" `attr` elem
  return $ XEventCopy nm number ref

xerror :: Element i -> Maybe XDecl
xerror elem = do
  nm <- "name" `attr` elem
  number_string <- "number" `attr` elem
  number <- maybeRead number_string
  let fields = mapMaybe go cnt
      Elem _ _ cnt = elem
      go (CElem el _) = structField el
      go _ = Nothing
  guard $ not $ null fields
  return $ XError nm number fields

xercopy :: Element i -> Maybe XDecl
xercopy elem = do
  nm <- "name" `attr` elem
  number_string <- "number" `attr` elem
  number <- maybeRead number_string
  ref <- "ref" `attr` elem
  return $ XErrorCopy nm number ref

xstruct :: Element i -> Maybe XDecl
xstruct elem = case attr "name" elem of
    Nothing -> Nothing
    Just nm -> 
        let fields = mapMaybe f cnt
            Elem _ _ cnt = elem

            f (CElem el _) = structField el
            f _ = Nothing
        in case fields of
             [] -> Nothing
             _  -> Just $ XStruct nm fields

xunion :: Element i -> Maybe XDecl
xunion = const Nothing

xidtype :: Element i -> Maybe XDecl
xidtype elem = do
  name <- "name" `attr` elem
  return $ XidType name

xidunion :: Element i -> Maybe XDecl
xidunion elem = do
  nm <- "name" `attr` elem
  let types = mapMaybe go cnt
      Elem _ _ cnt = elem
      go (CElem el _) = unionElem el
      go _ = Nothing
  guard $ not $ null types
  return $ XidUnion nm types

unionElem :: Element i -> Maybe UnionElem
unionElem elem | elem `named` "type" = do
                   let (Elem _ _ [CString _ str _]) = elem
                   return $ UnionElem str
unionElem _ = Nothing

xtypedef :: Element i -> Maybe XDecl
xtypedef elem = do
  oldname <- "oldname" `attr` elem
  newname <- "newname" `attr` elem
  return $ XTypeDef newname oldname


-- |If the passed-in element can be interpretted as a
-- struct element, it will be.
structField :: Element i -> Maybe StructElem
structField elem
    | elem `named` "field" = do
        typ <- "type" `attr` elem
        name <- "name" `attr` elem
        return $ SField name typ

    | elem `named` "pad" = do
        bytes <- "bytes" `attr` elem
        byte_count <- maybeRead bytes
        return $ Pad byte_count

    | elem `named` "list" = do
        typ <- "type" `attr` elem
        name <- "name" `attr` elem
        celem <- firstChildElem elem
        expr <- expression celem
        return $ List name typ expr

    | elem `named` "valueparam" = do
        mask_typ <- "value-mask-type" `attr` elem
        mask_name <- "value-mask-name" `attr` elem
        list_name <- "value-list-name" `attr` elem
        return $ ValueParam mask_typ mask_name list_name

    | elem `named` "exprfield" = do
        typ <- "type" `attr` elem
        name <- "name" `attr` elem
        celem <- firstChildElem elem
        expr <- expression celem
        return $ ExprField name typ expr

structField _ = Nothing

firstChildElem :: Element i -> Maybe (Element i)
firstChildElem (Elem _ _ cnt) = listToMaybe $ mapMaybe go cnt
    where go (CElem el _) = return el
          go _ = Nothing

firstTextElem :: Element i -> Maybe String
firstTextElem (Elem _ _ cnt) = listToMaybe $ mapMaybe go cnt
    where go (CString _ str _) = return str
          go _ = Nothing

-- |Interpret an element as an expression
expression :: Element i -> Maybe Expression
expression elem | elem `named` "fieldref"
                    = FieldRef `liftM` firstTextElem elem
                | elem `named` "value"
                    = firstTextElem elem >>= maybeValue
                | elem `named` "bit"
                    = firstTextElem elem >>= maybeBit
                | elem `named` "op" = maybeOp elem
expression _ = Nothing

-- probably doesn't handle hex values correctly
maybeValue :: String -> Maybe Expression
maybeValue str = Value `liftM` maybeRead str

maybeBit :: String -> Maybe Expression
maybeBit str = do
  n <- maybeRead str
  guard $ n >= 0
  return $ Bit n

maybeOp :: Element i -> Maybe Expression
maybeOp elem = do
  op_string <- "op" `attr` elem
  binop <- toBinop op_string
  let celems = childElements elem
  [exprLhs, exprRhs] <- mapM expression celems
  return $ Op binop exprLhs exprRhs

childElements :: Element i -> [Element i]
childElements (Elem _ _ cnt) = mapMaybe go cnt
    where go (CElem el _) = return el
          go _ = Nothing

toBinop :: String -> Maybe Binop
toBinop "+"  = return Add
toBinop "-"  = return Sub
toBinop "*"  = return Mult
toBinop "/"  = return Div
toBinop "&"  = return And
toBinop "&amp;" = return And
toBinop ">>" = return RShift
toBinop _ = Nothing

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

-- stolen from Network.CGI.Protocol
-- Probably more forgiving than I would've done it.
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
