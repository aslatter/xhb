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
  AttValue [Left atv] <- snd `liftM` L.find ((== nm) . fst) ats
  return atv

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
extractDecls (Elem _ _ cnt) = mapMaybe xdecls cnt
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
   xdecls _ = Nothing

xrequest :: Element i -> Maybe XDecl
xrequest elem = do
  nm <- "name" `attr` elem
  code_string <- "opcode" `attr` elem
  code <- maybeRead code_string
  let fields = mapMaybe go cnt
      Elem _ _ cnt = elem
      go (CElem el _) = structField el
      go _ = Nothing
  guard $ not $ null fields
  let reply = getReply elem
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
xevcopy = const Nothing

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
xercopy = const Nothing

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

    | elem `named` "list" = Nothing
structField _ = Nothing

-- stolen from Network.CGI.Protocol
-- Probably more forgiving than I would've done it.
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
