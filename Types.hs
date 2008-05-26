module Types where


import qualified Data.List as L
import Control.Monad

data XHeader = XHeader Name (Maybe ExInfo) [XDecl]

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

data StructElem = Pad Int
                | ListSize Name Type String
                | List Name Type String
                | SField Name Type

type Name = String
type Type = String
type XReply = [StructElem]
type Ref = String

data ExInfo = ExInfo Name Name Version

type Version = (String,String)

data UnionElem = UnionElem Type

data EnumElem = EnumElem Name Int
