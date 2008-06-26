{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

module XHB.Shared where

import Data.Binary.Put
import Data.Binary.Get

import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe

import Control.Monad
import Control.Exception
import Data.Function

import Data.List as L

import Foreign.C.Types (CChar)

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

type BOOL = Word8

type BYTE = Word8

newtype Xid = MkXid Word32
 deriving (Eq, Ord, Serialize, Deserialize)

instance Show Xid where
    show (MkXid x) = show x

class XidLike a where
    fromXid :: Xid -> a
    toXid   :: a -> Xid

instance XidLike Xid where
    fromXid = id
    toXid   = id

-- Enums and ValueParams

class SimpleEnum a where
    toValue :: Num n => a -> n
    fromValue :: Num n => n -> a

class BitEnum a where
    toBit :: a -> Int
    fromBit :: Int -> a

fromMask :: (Bits b, BitEnum e) => b -> [e]
fromMask x = mapMaybe go [0..(bitSize x) - 1]
    where go i | x `testBit` i = return $ fromBit i
               | otherwise = Nothing

toMask :: (Bits b, BitEnum e) => [e] -> b
toMask = foldl' (.&.) 0 . map (bit . toBit)


data ValueParam a = VP a [Word32]

toValueParam :: (Bits a, BitEnum e) => [(e,Word32)] -> ValueParam a
toValueParam xs = 
    let (es,ws) = unzip $ L.sortBy (compare `on` toBit . fst) xs
    in VP (toMask es) ws

fromValueParam :: (Bits a, BitEnum e) => ValueParam a -> [(e,Word32)]
fromValueParam (VP x ws) =
    let es = fromMask x
    in assert (length es == length ws) $ zip es ws


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
    size :: a -> Int -- Size in bytes

class Deserialize a where
    deserialize :: BO -> Get a


deserializeList :: Deserialize a => BO -> Int -> Get [a]
deserializeList bo n = go n
    where go 0 = return []
          go n = do
            x <- deserialize bo
            xs <- go (n-1)
            return $ x : xs

serializeList :: Serialize a => BO -> [a] -> Put
serializeList bo = mapM_ $ serialize bo

--Instances


-- Words
instance Serialize Word8 where
    serialize _ = putWord8
    size _ = 1

instance Deserialize Word8 where
    deserialize _ = getWord8

instance Serialize Word16 where
    serialize BE = putWord16be
    serialize LE = putWord16le

    size _ = 2

instance Deserialize Word16 where
    deserialize BE = getWord16be
    deserialize LE = getWord16le
    

instance Serialize Word32 where
    serialize BE = putWord32be
    serialize LE = putWord32le

    size _ = 4

instance Deserialize Word32 where
    deserialize BE = getWord32be
    deserialize LE = getWord32le

-- Ints
instance Serialize Int8 where
    serialize _ = putInt8
    size _ = 1

instance Deserialize Int8 where
    deserialize _ = getInt8


instance Serialize Int16 where
    serialize BE = putInt16be
    serialize LE = putInt16le

    size _ = 2

instance Deserialize Int16 where
    deserialize BE = getInt16be
    deserialize LE = getInt16le


instance Serialize Int32 where
    serialize BE = putInt32be
    serialize LE = putInt32le

    size _ = 4

instance Deserialize Int32 where
    deserialize BE = getInt32be
    deserialize LE = getInt32le

instance Serialize CChar where
    serialize _ = putWord8 . fromIntegral -- assumes a CChar is one word
    size _ = 1

instance Deserialize CChar where
    deserialize _ = liftM fromIntegral getWord8



-- Binary.Missing
putInt8 :: Int8 -> Put
putInt8 = putWord8 . fromIntegral

getInt8 :: Get Int8
getInt8 = liftM fromIntegral getWord8

putInt16be :: Int16 -> Put
putInt16be = putWord16be . fromIntegral

putInt16le :: Int16 -> Put
putInt16le = putWord16le . fromIntegral

getInt16be :: Get Int16
getInt16be = liftM fromIntegral getWord16be

getInt16le :: Get Int16
getInt16le = liftM fromIntegral getWord16le

putInt32be :: Int32 -> Put
putInt32be = putWord32be . fromIntegral

putInt32le :: Int32 -> Put
putInt32le = putWord32le . fromIntegral

getInt32be :: Get Int32
getInt32be = liftM fromIntegral getWord32be

getInt32le :: Get Int32
getInt32le = liftM fromIntegral getWord32le



-- Other
instance Serialize BO where
    serialize _ BE = putWord8 $ fromIntegral $ fromEnum '\102'
    serialize _ LE = putWord8 $ fromIntegral $ fromEnum '\154'
    
    size _ = 1

instance (Serialize a, Bits a) => Serialize (ValueParam a) where
    serialize bo (VP mask xs) = do
      serialize bo mask
      assert (length xs == setBits mask) $ return ()
      serializeList bo xs

    size (VP mask xs) = size mask + sum (map size xs)

instance (Deserialize a, Bits a) => Deserialize (ValueParam a) where
    deserialize bo = do
      mask <- deserialize bo
      let n = setBits mask
      xs <- deserializeList bo n
      return $ VP mask xs

-- |Returns the number of bits set in the passed-in
-- bitmask.
setBits :: Bits a => a -> Int
setBits a = foldl' go 0 [0 .. (bitSize a) - 1]
    where go !n bit | a `testBit` bit = n + 1
                    | otherwise = n


putSkip :: Int -> Put
putSkip n = replicateM_ n $ putWord8 0

---
--- A monad for serilization
