module FromXML (fromString) where

import Types
import Pretty

import Text.XML.Light

import Data.List as List
import qualified Data.Map as M
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Data.Monoid

fromString :: String -> XHeader
fromString str = fromJust $ do 
    el@(Element qname ats cnt _) <- parseXMLDoc str
    guard $ el `named` "xcb"
    modName <- el `attr` "header"
    let exinfo = extractExInfo el
    return $ XHeader modName exinfo $ extractDecls cnt

extractExInfo :: Element -> Maybe ExInfo
extractExInfo el = do
  xn <- el `attr` "extension-xname"
  n  <- el `attr` "extension-name"
  v1 <- el `attr` "major-version"
  v2 <- el `attr` "minor-version"
  return $ ExInfo n xn (v1,v2)

extractDecls :: [Content] -> [XDecl]
extractDecls = postProcess . mapMaybe declFromElem . onlyElems

declFromElem :: Element -> Maybe XDecl
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
    | otherwise = Nothing

xenum :: Element -> Maybe XDecl
xenum elem = do
  nm <- elem `attr` "name"
  let fields = mapMaybe enumField $ elChildren elem
  guard $ not $ null fields
  return $ XEnum nm fields

enumField :: Element -> Maybe EnumElem
enumField elem = do
  guard $ elem `named` "item"
  name <- elem `attr` "name"
  expr <- firstChild elem >>= expression
  return $ EnumElem name expr

xrequest :: Element -> Maybe XDecl
xrequest elem = do
  nm <- elem `attr` "name"
  code <- elem `attr` "opcode" >>= maybeRead
  let fields = mapMaybe structField $ elChildren elem
      reply = getReply elem
  guard $ not (null fields) || not (isNothing reply)
  return $ XRequest nm code fields reply

getReply :: Element -> Maybe XReply
getReply elem = do
  childElem <- unqual "reply" `findChild` elem
  let fields = mapMaybe structField $ elChildren childElem
  guard $ not $ null fields
  return fields

xevent :: Element -> Maybe XDecl
xevent elem = do
  name <- elem `attr` "name"
  number <- elem `attr` "number" >>= maybeRead
  let fields = mapMaybe structField $ elChildren elem
  guard $ not $ null fields
  return $ XEvent name number fields

xevcopy :: Element -> Maybe XDecl
xevcopy elem = do
  name <- elem `attr` "name"
  number <- elem `attr` "number" >>= maybeRead
  ref <- elem `attr` "ref"
  return $ XEventCopy name number ref

xerror :: Element -> Maybe XDecl
xerror elem = do
  name <- elem `attr` "name"
  number <- elem `attr` "number" >>= maybeRead
  let fields = mapMaybe structField $ elChildren elem
  guard $ not $ null fields
  return $ XError name number fields

xercopy :: Element -> Maybe XDecl
xercopy elem = do
  name <- elem `attr` "name"
  number <- elem `attr` "number" >>= maybeRead
  ref <- elem `attr` "ref"
  return $ XErrorCopy name number ref

xstruct :: Element -> Maybe XDecl
xstruct elem = do
  name <- elem `attr` "name"
  let fields = mapMaybe structField $ elChildren elem
  guard $ not $ null fields
  return $ XStruct name fields

xunion :: Element -> Maybe XDecl
xunion _elem = Nothing

xidtype :: Element -> Maybe XDecl
xidtype elem = liftM XidType $ elem `attr` "name"

xidunion :: Element -> Maybe XDecl
xidunion elem = do
  name <- elem `attr` "name"
  let types = mapMaybe unionElem $ elChildren elem
  guard $ not $ null types
  return $ XidUnion name types

unionElem :: Element -> Maybe UnionElem
unionElem elem = do
  guard $ elem `named` "type"
  return $ UnionElem $ strContent elem

xtypedef :: Element -> Maybe XDecl
xtypedef elem = do
  oldname <- elem `attr` "oldname"
  newname <- elem `attr` "newname"
  return $ XTypeDef newname oldname


structField :: Element -> Maybe StructElem
structField elem
    | elem `named` "field" = do
        typ <- elem `attr` "type"
        name <- elem `attr` "name"
        return $ SField name typ

    | elem `named` "pad" = do
        bytes <- elem `attr` "bytes" >>= maybeRead
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

    | elem `named` "reply" = Nothing -- handled separate

    | otherwise = let name = elName elem
                  in error $ "I don't know what to do with structelem "
 ++ show name

expression :: Element -> Maybe Expression
expression elem | elem `named` "fieldref"
                    = return $ FieldRef $ strContent elem
                | elem `named` "value"
                    = Value `liftM` maybeRead (strContent elem)
                | elem `named` "bit"
                    = Bit `liftM` do
                        n <- maybeRead (strContent elem)
                        guard $ n >= 0
                        return n
                | elem `named` "op" = do
                    binop <- elem `attr` "op" >>= toBinop
                    [exprLhs,exprRhs] <- mapM expression $ elChildren elem
                    return $ Op binop exprLhs exprRhs

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
               
  

----
----
-- Utility functions
----
----

firstChild :: Element -> Maybe Element
firstChild = listToMaybe . elChildren

named :: Element -> String -> Bool
named (Element qname _ _ _) name | qname == unqual name = True
named _ _ = False

attr :: Element -> String -> Maybe String
(Element _ xs _ _) `attr` name = do 
                Attr _ res <- List.find p xs
                return res
    where p (Attr qname _) | qname == unqual name = True
          p _ = False

-- stolen from Network.CGI.Protocol
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
