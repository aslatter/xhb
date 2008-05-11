module Types where

import Text.XML.HaXml.Xml2Haskell

data XHeader = XHeader Name (Maybe ExInfo) [XDecl]

data XDecl = XStruct  Name [StructElem]
           | XTypeDef Name Type
           | XEvent Name Int [StructElem]
           | XRequest Name Int [StructElem] (Maybe XReply)
           | XidType  Name
           | XidUnion  Name [UnionElem]
           | XEnum [EnumElem]
           | XImport Name

data StructElem = Pad Int
                | ListSize Name Type String
                | List Name Type String
                | SField Name Type

type Name = String
type Type = String
type XReply = [StructElem]

data ExInfo = ExInfo Name Name Version

type Version = (String,String)

data UnionElem = UnionElem Type

data EnumElem = EnumElem Name Int


fromElem :: Element -> XHeader
fromElem el|el `named` "xcb" =
         let Just modName = attr "header" el
             exinfo = extractExInfo el
             decs = extractDecls el
         in XHeader modName exinfo decs
fromElem _  = error "No parse in Types.fromElem"


attr :: String -> Element -> Maybe String
attr nm (Elem _ ats _) = attr' ats
 where attr' ((nm',Left str):ats)|nm==nm' = Just str
                                 |otherwise = attr' ats
       attr' ((_,Right {}):ats) = attr' ats
       attr' _ = Nothing


named :: Element -> String -> Bool
named (Elem nm _ _) nm'| nm == nm' = True
named _ _ = False

extractExInfo :: Element -> Maybe ExInfo
extractExInfo el = do
  xn <- attr "extension-xname" el
  n  <- attr "extension-name"  el
  v1 <- attr "major-version"   el
  v2 <- attr "minor-version"   el
  return $ ExInfo n xn (v1,v2)

extarctDecls :: Element -> [XDecl]
extarctDecls (Elem _ _ cnt) = foldr xdecls [] cnt
where
 xdecls (Celem elem) xs
     |elem `named` "import" = imp elem  : xs
     |elem `named` "enum"   = enum elem : xs
     |elem `named` "struct" = struct elem : xs
     |elem `named` "xidtype" = xidtype elem : xs
     |elem `named` "xidunion" = xidunion elem : xs
     |elem `named` "typedef" = typdef elem : xs
     |elem `named` "event" = event elem : xs
     |elem `named` "eventcopt" = eventcopy elem cnt : xs
     |elem