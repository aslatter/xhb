module Types where


import qualified Data.List as L
import Control.Monad

data XHeader = XHeader Name (Maybe ExInfo) [XDecl]
 deriving (Show)

data XDecl = XStruct  Name [StructElem]
           | XTypeDef Name Type
           | XEvent Name Int [StructElem]
           | XRequest Name Int [StructElem] (Maybe XReply)
           | XidType  Name
           | XidUnion  Name [UnionElem]
           | XEnum [EnumElem]
           | XImport Name
           | XError Name Int [StructElem] -- check to make sure
           | XEventCopy Name Int Ref  -- should not appear after post processing
           | XErrorCopy Name Int Ref  -- should not appear after post processing
 deriving (Show)

data StructElem = Pad Int
                | ListSize Name Type String
                | List Name Type Expression
                | SField Name Type
 deriving (Show)

type Name = String
type Type = String
type XReply = [StructElem]
type Ref = String

data ExInfo = ExInfo Name Name Version
 deriving (Show)

type Version = (String,String)

data UnionElem = UnionElem Type
 deriving (Show)

data EnumElem = EnumElem Name Int
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
