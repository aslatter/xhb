module Types where


import qualified Data.List as L
import Control.Monad

data XHeader = XHeader {xheader_name :: Name
                       ,xheader_exinfo :: (Maybe ExInfo)
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
                | List Name Type Expression
                | SField Name Type
                | ExprField Name Type Expression
                | ValueParam Type MaskName ListName
 deriving (Show)

type Name = String
type Type = String
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
data EnumElem = EnumElem Name Expression
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

