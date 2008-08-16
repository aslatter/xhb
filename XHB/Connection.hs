module XHB.Connection
    (Connection
    ,mkConnection
    ,newResource
    ,pollForEvent
    ,waitForEvent
    ,pollForError
    ,waitForError
    ,setCrashOnError
    ,RawEvent
    ,RawError
    ,connectionSetup
    ,getRoot
    ,getReply
    )
    where

import Data.Word

-- MAY import generated type modules (XHB.Gen.*.Types)
-- MAY NOT import other generated modules

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad

import System.IO

import Foreign.C.String

import Data.Maybe

import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS

import Data.Binary.Get
import Data.Binary.Put

import Data.Bits

import XHB.Gen.Xproto.Types

import XHB.Connection.Types
import XHB.Connection.Internal
import XHB.Shared

-- | Returns the 'Setup' information returned by the server
-- during the initiation of the connection.
connectionSetup :: Connection -> Setup
connectionSetup = conf_setup . conn_conf

newResource :: XidLike a => Connection -> IO a
newResource c = do
  xidM <- nextXid c
  case xidM of
    Just xid -> return . fromXid $ xid
    Nothing -> error "resource ids exhausted" -- request more here

nextXid :: Connection -> IO (Maybe Xid)
nextXid c = atomically $ do
              let tv =  conn_resource_ids c
              xids <- readTVar tv
              case xids of
                [] -> return Nothing
                (x:xs) -> do
                   writeTVar tv xs
                   return . return $ x


pollForEvent :: Connection -> IO (Maybe RawEvent)
pollForEvent c = atomically $ pollTChan $ conn_event_queue c

waitForEvent :: Connection -> IO RawEvent
waitForEvent c = atomically $ readTChan $ conn_event_queue c
                
pollForError :: Connection -> IO (Maybe RawError)
pollForError c = atomically $ pollTChan $ conn_error_queue c

waitForError :: Connection -> IO RawError
waitForError c = atomically $ readTChan $ conn_error_queue c

pollTChan :: TChan a -> STM (Maybe a)
pollTChan tc = do
  empty <- isEmptyTChan tc
  if empty then return Nothing
   else Just `liftM` readTChan tc

getReply :: Receipt a -> IO (Either RawError a)
getReply r = atomically $ do
               a <- takeTMVar r
               putTMVar r a
               return a


-- | If you don't feel like writing error handlers, but at least want to know that
-- one happened for debugging purposes, call this to have execution come to an
-- abrupt end if an error is received.
setCrashOnError :: Connection -> IO ()
setCrashOnError c = do
  forkIO $ do
    waitForError c
    error "Received error from server.  Crashing."
  return ()


-- Any response from the server is first read into
-- this type.
data GenericReply = GenericReply
    {grep_response_type :: ResponseType
    ,grep_pad :: Word8
    ,grep_sequence :: Word16
    ,grep_reply_length :: Word32 -- only useful for replies
    }

data ResponseType
    = ResponseTypeEvent Word8
    | ResponseTypeError
    | ResponseTypeReply

instance Deserialize GenericReply where
    deserialize bo = do
      type_flag <- deserialize bo
      let rType = case type_flag of
                    0 -> ResponseTypeError
                    1 -> ResponseTypeReply
                    _ -> ResponseTypeEvent type_flag
      pad <- deserialize bo
      sequence <- deserialize bo
      reply_length <- deserialize bo
      return $ GenericReply rType pad sequence reply_length



-- state maintained by the read loop
data ReadLoop = ReadLoop
    {read_error_queue :: TChan RawError -- write only
    ,read_event_queue :: TChan RawEvent -- write only
    ,read_input_queue :: Handle -- read only
    ,read_reps :: TChan PendedReply -- read only
    ,read_config :: ConnectionConfig
    }


deserializeInReadLoop rl = deserialize (conf_byteorder $ read_config $ rl)

readBytes :: ReadLoop -> Int -> IO ByteString
readBytes rl n = BS.hGet (read_input_queue rl) n

-- the read loop slurps bytes off of the handle, and places
-- them into the appropriate shared structure.
readLoop :: ReadLoop -> IO ()
readLoop rl = do
  chunk <- readBytes rl 32
  let genRep = flip runGet chunk $ deserialize
               (conf_byteorder $ read_config $ rl)
  case grep_response_type genRep of
    ResponseTypeError -> readLoopError rl genRep chunk
    ResponseTypeReply -> readLoopReply rl genRep chunk
    ResponseTypeEvent _ -> readLoopEvent rl genRep chunk
  readLoop rl

-- handle a response to a request
readLoopReply :: ReadLoop -> GenericReply -> ByteString -> IO ()
readLoopReply rl genRep chunk = do

  -- grab the rest of the response bytes
  let rlength = grep_reply_length genRep
  extra <- readBytes rl $ fromIntegral $ 4 * rlength

  -- deserialize the response
  let bytes = chunk `BS.append` extra

  -- place the response into the pending reply TMVar, or discard it
  atomically $ do
    nextPend <- readTChan $ read_reps rl
    if (pended_sequence nextPend) == (grep_sequence genRep)
     then case pended_reply nextPend of
            WrappedReply replyHole -> 
                let reply = flip runGet bytes $ deserializeInReadLoop rl
                in putTMVar replyHole $ Right reply
     else unGetTChan (read_reps rl) nextPend


-- take the bytes making up the error response, shove it in
-- a queue.
--
-- If the error corresponds to one of the pending replies,
-- place the error into the pending reply TMVar instead.
readLoopError rl genRep chunk = do
  atomically $ do
    nextPend <- readTChan $ read_reps rl
    if (pended_sequence nextPend) == (grep_sequence genRep)
     then case pended_reply nextPend of
            WrappedReply replyHole -> putTMVar replyHole $ Left chunk
     else do
       unGetTChan (read_reps rl) nextPend
       writeTChan (read_error_queue rl) chunk

-- take the bytes making up the event response, shove it in
-- a queue
readLoopEvent rl genRep chunk =
    atomically $ writeTChan (read_event_queue rl) chunk

-- Handshake with the server
-- parse result of handshake
-- launch the thread which holds the handle for reading
mkConnection :: Handle -> IO (Maybe Connection)
mkConnection hnd = do
  errorQueue <- newTChanIO
  eventQueue <- newTChanIO
  replies <- newTChanIO
  sequence <- initialSequence

  wrappedHandle <- newMVar hnd 

  confM <- handshake hnd
  if isNothing confM then return Nothing else do
  let Just conf = confM

  rIds <- newTVarIO $ resourceIds conf

  let rlData = ReadLoop errorQueue eventQueue hnd replies conf
  
  readTid <- forkIO $ readLoop rlData

  return $ Just $ 
     Connection
      errorQueue
      eventQueue
      readTid
      wrappedHandle
      replies
      conf
      sequence
      rIds

resourceIds :: ConnectionConfig -> [Xid]
resourceIds cc = resourceIdsFromSetup $ conf_setup cc

resourceIdsFromSetup :: Setup -> [Xid]
resourceIdsFromSetup s =
    let base = resource_id_base_Setup s
        mask = resource_id_mask_Setup s
        max = mask
        step = mask .&. (-mask)
    in map MkXid $ map (.|. base) [0,step .. max]

-- first 8 bytes of the response from the setup request
data GenericSetup = GenericSetup
                  {setup_status :: SetupStatus
                  ,setup_length :: Word16
                  }
                    deriving Show

instance Deserialize GenericSetup where
    deserialize bo = do
      status <- deserialize bo
      skip 5
      length <- deserialize bo
      return $ GenericSetup status length 

data SetupStatus = SetupFailed | SetupAuthenticate | SetupSuccess
 deriving Show

instance Deserialize SetupStatus where
    deserialize bo = wordToStatus `liftM` deserialize bo
        where wordToStatus :: Word8 -> SetupStatus
              wordToStatus 0 = SetupFailed
              wordToStatus 1 = SetupSuccess
              wordToStatus 2 = SetupAuthenticate
              wordToStatus n = error $ 
                     "Unkonwn setup status flag: " ++ show n

-- send the setup request to the server,
-- receive the setup response
handshake :: Handle -> IO (Maybe ConnectionConfig)
handshake hnd = do

  let bo = BE

  -- send setup request

  let requestChunk =  runPut $ serialize bo $ setupRequest bo
  BS.hPut hnd $ requestChunk

  -- grab an 8-byte chunk to get the response type and size
  firstChunk <- BS.hGet hnd 8

  let genSetup = runGet (deserialize bo) firstChunk

  -- grab the rest of the setup response
  secondChunk <- BS.hGet hnd $ fromIntegral $ (4 *) $ setup_length genSetup
  let setupBytes = firstChunk `BS.append` secondChunk

  -- handle the response type
  case setup_status genSetup of
    SetupFailed -> do
       let failed = runGet (deserialize bo) setupBytes
           failMessage = map castCCharToChar (reason_SetupFailed failed)
       hPutStrLn stderr failMessage
       return Nothing
    SetupAuthenticate -> do
       let auth = runGet (deserialize bo) setupBytes
           authMessage = map castCCharToChar (reason_SetupAuthenticate auth)
       hPutStrLn stderr authMessage
       return Nothing
    SetupSuccess -> do
       let setup = runGet (deserialize bo) setupBytes
       return . return $ ConnectionConfig bo setup
       
padBS n = BS.replicate n 0

initialSequence :: IO (TVar SequenceId)
initialSequence = newTVarIO 1

setupRequest :: BO -> SetupRequest
setupRequest bo = MkSetupRequest
                  (fromIntegral $ byteOrderToNum bo)
                  11 -- major version
                  0  -- minor version
                  0 -- auth name length
                  0 -- auth data length
                  [] -- auth name
                  [] -- auth data

                  

getRoot :: Connection -> WINDOW
getRoot = root_SCREEN . head . roots_Setup . conf_setup . conn_conf