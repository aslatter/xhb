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

import Control.Concurrent.STM

import System.ByteOrder

-- crazy imports for put/get storable
import qualified Data.ByteString.Internal as Strict
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr

import System.IO.Unsafe ( unsafePerformIO )

byteOrderToNum :: ByteOrder -> Int
byteOrderToNum BigEndian = fromEnum '\o102' -- B
byteOrderToNum LittleEndian = fromEnum '\o154' -- l
byteOrderToNum Mixed{} = error "Mixed endian platforms not supported."

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

instance XidLike Word32 where
    fromXid (MkXid w) = w
    toXid = MkXid

xidNone :: Xid
xidNone = MkXid 0

-- Enums and ValueParams

class SimpleEnum a where
    toValue :: (Eq n, Num n) => a -> n
    fromValue :: (Eq n, Num n) => n -> a

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

toMask :: (Bits b, Num b, BitEnum e) => [e] -> b
toMask = foldl' (.|.) 0 . map (bit . toBit)


data ValueParam a = VP a [Word32]

toValueParam :: (Bits a, Num a, BitEnum e) => [(e,Word32)] -> ValueParam a
toValueParam xs = 
    let (es,ws) = unzip $ L.sortBy (compare `on` toBit . fst) xs
    in VP (toMask es) ws

fromValueParam :: (Bits a, BitEnum e) => ValueParam a -> [(e,Word32)]
fromValueParam (VP x ws) =
    let es = fromMask x
    in assert (length es == length ws) $ zip es ws

emptyValueParam :: Num a => ValueParam a
emptyValueParam = VP 0 []

instance (Bits a, Show a) => Show (ValueParam a) where
    show v = show (fromValueParam v :: [(Integer,Word32)])

stringToCList :: String -> [CChar]
stringToCList = map castCharToCChar


class Serialize a where
    serialize :: a -> Put
    size :: a -> Int -- Size in bytes

class Deserialize a where
    deserialize :: Get a

class ExtensionRequest a where
    serializeRequest :: a -> RequestOpCode -> Put
    extensionId :: a -> ExtensionId

type RequestOpCode = Word8
type ExtensionId = String -- limited to ASCII
  

-- In units of four bytes
type ReplyLength = Word32

-- The Receipt type allows the sender of the request
-- to arbitrarily munge the result before handing
-- it back to the caller
newtype Receipt a = MkReceipt (TVar (InnerReceipt a))

type RawReceipt = TMVar (Either SomeError ByteString)

data InnerReceipt a
    = Item (Either SomeError a)
    | Result RawReceipt (ByteString -> a)

newEmptyReceipt :: (ByteString -> a) -> IO (Receipt a, RawReceipt)
newEmptyReceipt f = do
  rawReceipt <- newEmptyTMVarIO
  ref <- newTVarIO $ Result rawReceipt f
  return $ (MkReceipt ref, rawReceipt)

newDeserReceipt :: Deserialize a => IO (Receipt a, RawReceipt)
newDeserReceipt = newEmptyReceipt $ runGet deserialize

putReceipt :: RawReceipt -> Either SomeError ByteString -> STM ()
putReceipt = putTMVar

-- | Extracts a reply from the receipt from the request.
-- Blocks until the reply is available.
getReply :: Receipt a -> IO (Either SomeError a)
getReply (MkReceipt ref) = atomically $
    readTVar ref >>= \ircpt -> case ircpt of
       Item a -> return a
       Result rrcpt f -> do
         res <- takeTMVar rrcpt
         let ret = either Left (Right . f) res
         writeTVar ref $ Item ret
         return ret

-- Because new errors and events are introduced with each extension,
-- I don't want to give the users of this library pattern-match
-- error every time a new extension is added.

class (Typeable a, Show a) => Error a where
    fromError :: SomeError -> Maybe a
    toError :: a -> SomeError

    fromError (SomeError e) = cast e
    toError = SomeError

data SomeError = forall a . Error a => SomeError a

instance Show SomeError where
    show se = case se of
          SomeError err -> show err

data UnknownError = UnknownError BS.ByteString deriving (Typeable, Show)
instance Error UnknownError



class Typeable a => Event a where
    fromEvent :: SomeEvent -> Maybe a
    toEvent :: a -> SomeEvent

    fromEvent (SomeEvent e) = cast e
    toEvent = SomeEvent

data SomeEvent = forall a . Event a => SomeEvent a

data UnknownEvent = UnknownEvent BS.ByteString deriving (Typeable)
instance Event UnknownEvent





deserializeList :: Deserialize a => Int -> Get [a]
deserializeList n = go n
    where go 0 = return []
          go n = do
            x <- deserialize
            xs <- go (n-1)
            return $ x : xs

serializeList :: Serialize a => [a] -> Put
serializeList = mapM_ serialize

convertBytesToRequestSize n =
    fromIntegral $ case quotRem n 4 of
      (d,0) -> d
      (d,r) -> d + 1

requiredPadding n = 
    fromIntegral $ case quotRem n 4 of
      (_,0) -> 0
      (_,r) -> 4 - r

--Instances


instance Serialize Bool where
    serialize = serialize `fmap` boolToWord
    size = size . boolToWord

instance Deserialize Bool where
    deserialize = wordToBool `fmap` deserialize

boolToWord :: Bool -> Word8
wordToBool :: Word8 -> Bool

boolToWord True = 1
boolToWord False = 0

wordToBool 0 = False
wordToBool _ = True


-- Words
instance Serialize Word8 where
    serialize = putWord8
    size _ = 1

instance Deserialize Word8 where
    deserialize = getWord8

instance Serialize Word16 where
    serialize = putWord16host
    size _ = 2

instance Deserialize Word16 where
    deserialize = getWord16host


instance Serialize Word32 where
    serialize = putWord32host
    size _ = 4

instance Deserialize Word32 where
    deserialize = getWord32host

-- Ints
instance Serialize Int8 where
    serialize = putInt8
    size _ = 1

instance Deserialize Int8 where
    deserialize = getInt8


instance Serialize Int16 where
    serialize = putInt16host
    size _ = 2

instance Deserialize Int16 where
    deserialize = getInt16host


instance Serialize Int32 where
    serialize = putInt32host
    size _ = 4

instance Deserialize Int32 where
    deserialize = getInt32host


instance Serialize CChar where
    serialize = putWord8 . fromIntegral -- assumes a CChar is one word
    size _ = 1

instance Deserialize CChar where
    deserialize = liftM fromIntegral getWord8



-- Binary.Missing

-- All of this relies on being able to roundtrip:
-- (IntN -> WordN) and (WordN -> IntN) using 'fromIntegral'

putInt8 :: Int8 -> Put
putInt8 = putWord8 . fromIntegral

getInt8 :: Get Int8
getInt8 = liftM fromIntegral getWord8

putInt16host :: Int16 -> Put
putInt16host = putWord16host . fromIntegral

getInt16host :: Get Int16
getInt16host = liftM fromIntegral getWord16host

putInt32host :: Int32 -> Put
putInt32host = putWord32host . fromIntegral

getInt32host :: Get Int32
getInt32host = liftM fromIntegral getWord32host

-- Fun stuff

-- I've no idea if this is what the other end expects
instance Deserialize CFloat where
    deserialize = getStorable

instance Serialize CFloat where
    size x = sizeOf x
    serialize = putStorable

instance Deserialize CDouble where
    deserialize = getStorable


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
                       Int -> ValueParam a -> Put
serializeValueParam pad (VP mask xs) = do
  serialize mask
  putSkip pad
  assert (length xs == setBits mask) $ return ()
  serializeList xs
  

instance (Deserialize a, Bits a) => Deserialize (ValueParam a) where
    deserialize = deserializeValueParam 0

deserializeValueParam :: (Deserialize a, Bits a) =>
                         Int -> Get (ValueParam a)
deserializeValueParam pad = do
  mask <- deserialize
  skip pad
  let n = setBits mask
  xs <- deserializeList n
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

isCard32 :: Word32 -> a
isCard32 = undefined
