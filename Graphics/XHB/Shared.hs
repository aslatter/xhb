{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

module Graphics.XHB.Shared where

-- MAY NOT import any gnerated files

import Data.Typeable

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

import Foreign.C.Types (CChar, CFloat, CDouble)
import Foreign.C.String

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)

import Control.Concurrent.STM (TMVar)

-- crazy imports for put/get storable
import qualified Data.ByteString.Internal as Strict
import Foreign
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr


-- |Byte-ordering.
data BO = BE -- ^Big-endian
        | LE -- ^Little-endian
 deriving (Show, Ord, Eq)

byteOrderToNum :: BO -> Int
byteOrderToNum BE = fromEnum '\o102' -- B
byteOrderToNum LE = fromEnum '\o154' -- l

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

instance BitEnum Integer where
    toBit = fromIntegral
    fromBit = fromIntegral

fromMask :: (Bits b, BitEnum e) => b -> [e]
fromMask x = mapMaybe go [0..(bitSize x) - 1]
    where go i | x `testBit` i = return $ fromBit i
               | otherwise = Nothing

toMask :: (Bits b, BitEnum e) => [e] -> b
toMask = foldl' (.|.) 0 . map (bit . toBit)


data ValueParam a = VP a [Word32]

toValueParam :: (Bits a, BitEnum e) => [(e,Word32)] -> ValueParam a
toValueParam xs = 
    let (es,ws) = unzip $ L.sortBy (compare `on` toBit . fst) xs
    in VP (toMask es) ws

fromValueParam :: (Bits a, BitEnum e) => ValueParam a -> [(e,Word32)]
fromValueParam (VP x ws) =
    let es = fromMask x
    in assert (length es == length ws) $ zip es ws

emptyValueParam :: Bits a => ValueParam a
emptyValueParam = VP 0 []

instance (Bits a, Show a) => Show (ValueParam a) where
    show v = show (fromValueParam v :: [(Integer,Word32)])

stringToCList :: String -> [CChar]
stringToCList = map castCharToCChar


class Serialize a where
    serialize :: BO -> a -> Put
    size :: a -> Int -- Size in bytes

class Deserialize a where
    deserialize :: BO -> Get a

class ExtensionRequest a where
    serializeRequest :: a -> RequestOpCode -> BO -> Put
    extensionId :: a -> ExtensionId

type RequestOpCode = Word8
type ExtensionId = String -- limited to ASCII
  

-- In units of four bytes
type ReplyLength = Word32

type Receipt a = TMVar (Either SomeError a)

-- Because new errors and events are introduced with each extension,
-- I don't want to give the users of this library pattern-match
-- error every time a new extension is added.

class Typeable a => Error a
data SomeError = forall a . Error a => SomeError a

fromError :: Error e => SomeError -> Maybe e
fromError (SomeError e)= cast e

data UnknownError = UnknownError BS.ByteString deriving (Typeable)
instance Error UnknownError



class Typeable a => Event a
data SomeEvent = forall a . Event a => SomeEvent a

fromEvent :: Event e => SomeEvent -> Maybe e
fromEvent (SomeEvent e) = cast e

data UnknownEvent = UnknownEvent BS.ByteString deriving (Typeable)
instance Event UnknownEvent





deserializeList :: Deserialize a => BO -> Int -> Get [a]
deserializeList bo n = go n
    where go 0 = return []
          go n = do
            x <- deserialize bo
            xs <- go (n-1)
            return $ x : xs

serializeList :: Serialize a => BO -> [a] -> Put
serializeList bo = mapM_ $ serialize bo

convertBytesToRequestSize n =
    fromIntegral $ case quotRem n 4 of
      (d,0) -> d
      (d,r) -> d + 1

requiredPadding n = 
    fromIntegral $ case quotRem n 4 of
      (_,0) -> 0
      (_,r) -> 4 - r

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

-- All of this relies on being able to roundtrip:
-- (IntN -> WordN) and (WordN -> IntN) using 'fromIntegral'

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

-- Fun stuff

-- this is wrong because we ignore byte-order.
-- oh well.
instance Deserialize CFloat where
    deserialize _ = getStorable

instance Serialize CFloat where
    size x = sizeOf x
    serialize _ x = putStorable x

instance Deserialize CDouble where
    deserialize _ = getStorable


getStorable :: Storable a => Get a
getStorable = (\dummy -> do
       let n = sizeOf dummy
       bytes <- getBytes n
       return $ storableFromBS bytes `asTypeOf` dummy
              ) undefined  

putStorable :: Storable a => a -> Put
putStorable = putByteString . bsFromStorable

storableFromBS (Strict.PS fptr len off) = 
    unsafePerformIO $ withForeignPtr fptr $ flip peekElemOff off . castPtr

bsFromStorable x = Strict.unsafeCreate (sizeOf x) $ \p -> do
                     poke (castPtr p) x

-- Other

instance (Serialize a, Bits a) => Serialize (ValueParam a) where
    serialize = serializeValueParam 0
    size (VP mask xs) = size mask + sum (map size xs)

-- there's one value param which needs funny padding, so it
-- uses the special function
serializeValueParam :: (Serialize a, Bits a) =>
                       Int -> BO -> ValueParam a -> Put
serializeValueParam pad bo (VP mask xs) = do
  serialize bo mask
  putSkip pad
  assert (length xs == setBits mask) $ return ()
  serializeList bo xs
  

instance (Deserialize a, Bits a) => Deserialize (ValueParam a) where
    deserialize = deserializeValueParam 0

deserializeValueParam :: (Deserialize a, Bits a) =>
                         Int -> BO -> Get (ValueParam a)
deserializeValueParam pad bo = do
  mask <- deserialize bo
  skip pad
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
putSkip 0 = return ()
putSkip n = replicateM_ n $ putWord8 0

isCard32 :: CARD32 -> a
isCard32 = undefined