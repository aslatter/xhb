
-- | This module contains functioanlity only for use
-- by other XHB modules, while still trying to ihde some
-- of the implementation details of the 'Connection'
-- data type.

module Graphics.XHB.Connection.Internal
    (sendRequest
    ,sendRequestWithReply
    ,byteOrderFromConn
    ,lookupExtension
    ,cacheExtension
    ,Connection
    ) where

import Data.Word(Word16)
import Control.Exception(bracket)

import Control.Concurrent.STM
import Control.Concurrent

import System.IO

import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS

import Data.Maybe
import qualified Data.Map as M

import Graphics.XHB.Connection.Types
import Graphics.XHB.Shared

import Graphics.XHB.Gen.Xproto.Types

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

-- | Lookup an extension in the extension cache.  Returns 'Nothing'
-- if queried extension is not cached
lookupExtension :: Connection -> ExtensionId -> IO (Maybe QueryExtensionReply)
lookupExtension c extId = atomically $ do
   m <- readTVar $ conn_extensions c
   return $ M.lookup extId m

-- | Add an extension to the extension cache.
cacheExtension :: Connection -> ExtensionId -> QueryExtensionReply -> IO ()
cacheExtension c extId ext = atomically $ do
   let tv = conn_extensions c

   m <- readTVar tv
   if isNothing (M.lookup extId m)
    then 
      let m' = M.insert extId ext m
      in writeTVar tv m'
    else return ()
