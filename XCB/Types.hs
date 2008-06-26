module XCB.Types where


import qualified Data.List as L
import Control.Monad

-- 'xheader_header' is the name gauranteed to exist, and is used in
-- imports and in type qualifiers.
--
-- 'xheader_name' is the InterCaps name, and should be prefered in the naming
-- of types, functions and haskell modules when available.
data XHeader = XHeader {xheader_header :: Name
                       ,xheader_xname :: Maybe Name
                       ,xheader_name :: Maybe Name
                       ,xheader_multiword :: Maybe Bool
                       ,xheader_major_version :: Maybe Int
                       ,xheader_minor_version :: Maybe Int
                       ,xheader_decls :: [XDecl]
                       }
 deriving (Show)

data XDecl = XStruct  Name [StructElem]
           | XTypeDef Name Type
           | XEvent Name Int [StructElem]
           | XRequest Name Int [StructElem] (Maybe XReply)
           | XidType  Name
           | XidUnion  Name [XidUnionElem]
           | XEnum Name [EnumElem]
           | XUnion Name [StructElem]
           | XImport Name
           | XError Name Int [StructElem]
 deriving (Show)

data StructElem = Pad Int
                | List Name Type (Maybe Expression)
                | SField Name Type
                | ExprField Name Type Expression
                | ValueParam Type MaskName ListName
 deriving (Show)

type Name = String
data Type = UnQualType Name | QualType Name Name deriving Show
type XReply = [StructElem]
type Ref = String

type MaskName = Name
type ListName = Name

data ExInfo = ExInfo Name Name Version
 deriving (Show)

type Version = (String,String)

data XidUnionElem = XidUnionElem Type
 deriving (Show)

-- Should only ever have expressions of type 'Value' or 'Bit'.
data EnumElem = EnumElem Name (Maybe Expression)
 deriving (Show)

data Expression = Value Int
                | Bit Int
                | FieldRef String
                | Op Binop Expression Expression
 deriving (Show)

data Binop = Add
           | Sub
           | Mult
           | Div
           | And
           | RShift
 deriving (Show)

