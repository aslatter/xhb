{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module XCB.Shared where

import Data.Binary.Put
import Data.Binary.Get

import Data.Word
import Data.Int

import Control.Monad

-- |Byte-ordering.
data BO = BE
        | LE
 deriving (Show, Ord, Eq)

type CARD8  = Word8
type CARD16 = Word16
type CARD32 = Word32

type INT32 = Int32
type INT16 = Int16
type INT8  = Int8

type BOOL = Bool

type BYTE = Word8

newtype Xid = MkXid Word32
 deriving (Eq, Ord, Show, Serialize, Deserialize)

class XidLike a where
    fromXid :: Xid -> a
    toXid   :: a -> Xid

instance XidLike Xid where
    fromXid = id
    toXid   = id

-- TEMPORARY FIX
type ClientMessageData = CARD32

{-
  I really don't know what endianness any
  of this should be, so I'm going to parameterize
  over byte-ordering.
 -}

data Config = MkConfig {byteorder :: BO
                       ,image_byteorder :: BO
                       }



class Serialize a where
    serialize :: BO -> a -> Put

class Deserialize a where
    deserialize :: BO -> Get a


deserializeList :: (Deserialize a, Num n) => BO -> n -> Get [a]
deserializeList bo 0 = return []
deserializeList bo n = liftM2 (:) (deserialize bo) (deserializeList bo $ n - 1)

--Instances

instance Serialize Word8 where
    serialize = const putWord8

instance Deserialize Word8 where
    deserialize = const getWord8


instance Serialize Word16 where
    serialize = pairBO (putWord16be, putWord16le)

instance Deserialize Word16 where
    deserialize = pairBO (getWord16be, getWord16le)


instance Serialize Word32 where
    serialize = pairBO (putWord32be, putWord32le)

instance Deserialize Word32 where
    deserialize = pairBO (getWord32be, getWord32le)

instance Serialize BO where
    serialize _ BE = putWord8 $ fromIntegral $ fromEnum '\102'
    serialize _ LE = putWord8 $ fromIntegral $ fromEnum '\154'

pairBO :: (a,a) -> BO -> a
pairBO t BE = fst t
pairBO t LE = snd t

putSkip :: Int -> Put
putSkip n = replicateM_ n $ putWord8 0
