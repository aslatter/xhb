module XHB.Connection.Internal where

import Data.Word(Word16)
import Control.Exception(bracket)

import Control.Concurrent.STM
import Control.Concurrent

import System.IO

import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS

import XHB.Connection.Types
import XHB.Shared

-- Assumes that the input bytestring is a properly formatted
-- and padded request.
sendRequest :: Connection -> ByteString -> IO ()
sendRequest c bytes = withConnectionHandle c $ \h -> do

  -- send bytes onto connection
  BS.hPut h bytes

  -- increment sequence
  _ <- atomically $ nextSequence c

  return ()

sendRequestWithReply :: Deserialize a => Connection -> ByteString -> Receipt a -> IO ()
sendRequestWithReply c bytes r = withConnectionHandle c $ \h -> do

  BS.hPut h bytes

  atomically $ do
    seq <- nextSequence c
    writeTChan (conn_reps c) $ PendedReply seq $ WrappedReply r

byteOrderFromConn = conf_byteorder . conn_conf 

-- | convert an extension request to a put action
{-
serializeExtensionRequest :: ExtRequest a => Connection -> a -> IO Put
serializeExtensionRequest c req = do
  extRep <- extensionInfo c $ extensionId req
  let present = extensioPresent extRep
      opCode = extensionOpCode extRep

      bo = byteOrderFromConn c

      putAction = serializeRequest req opCode bo

  return putAction
-}

-- Returns the next sequence ID
nextSequence :: Connection -> STM SequenceId
nextSequence c = do
  let tv = conn_next_sequence c
  seq <- readTVar tv
  writeTVar tv (seq + 1)
  return seq

-- Locks the handle for use by the passed in function.  Intended for
-- write access only.
--
-- NOTE: the read loop has a separate reference to the handle,
-- so it will not be blocked by this.
withConnectionHandle :: Connection -> (Handle -> IO a) -> IO a
withConnectionHandle c f = do
  let mv = conn_handle c
  bracket
     (takeMVar mv)
     (putMVar mv)
     f
{-# INLINE withConnectionHandle #-}

