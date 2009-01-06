{- |

Basic functions for initiating and working with a connection to an
X11 server.

-}

module Graphics.XHB.Connection
    (Connection
    ,connect
    ,connectTo
    ,displayInfo
    ,connectionSetup
    ,mkConnection
    ,newResource
    ,pollForEvent
    ,waitForEvent
    ,pollForError
    ,waitForError
    ,setCrashOnError
    ,SomeError
    ,SomeEvent
    ,getRoot
    )
    where

import Data.Word

-- MAY import generated type modules (XHB.Gen.*.Types)
-- MAY NOT import other generated modules

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad

import System.IO
import System.ByteOrder

import Foreign.C.String

import Data.List (genericLength)
import Data.Maybe
import Data.Monoid(mempty)
import qualified Data.Map as M

import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS

import Data.Binary.Get
import Data.Binary.Put

import Data.Bits

import Graphics.XHB.Gen.Xproto.Types
import Graphics.XHB.Gen.Extension

import Graphics.XHB.Connection.Types
import Graphics.XHB.Connection.Internal
import Graphics.XHB.Connection.Open
import Graphics.XHB.Shared

import Graphics.X11.Xauth

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


pollForEvent :: Connection -> IO (Maybe SomeEvent)
pollForEvent c = atomically $ pollTChan $ conn_event_queue c

waitForEvent :: Connection -> IO SomeEvent
waitForEvent c = atomically $ readTChan $ conn_event_queue c
                
pollForError :: Connection -> IO (Maybe SomeError)
pollForError c = atomically $ pollTChan $ conn_error_queue c

waitForError :: Connection -> IO SomeError
waitForError c = atomically $ readTChan $ conn_error_queue c

pollTChan :: TChan a -> STM (Maybe a)
pollTChan tc = do
  empty <- isEmptyTChan tc
  if empty then return Nothing
   else Just `liftM` readTChan tc


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
    ,grep_error_code :: Word8
    ,grep_sequence :: Word16
    ,grep_reply_length :: Word32 -- only useful for replies
    }

data ResponseType
    = ResponseTypeEvent Word8
    | ResponseTypeError
    | ResponseTypeReply

instance Deserialize GenericReply where
    deserialize = do
      type_flag <- deserialize
      let rType = case type_flag of
                    0 -> ResponseTypeError
                    1 -> ResponseTypeReply
                    _ -> ResponseTypeEvent type_flag
      code <- deserialize
      sequence <- deserialize
      reply_length <- deserialize
      return $ GenericReply rType code sequence reply_length



-- state maintained by the read loop
data ReadLoop = ReadLoop
    {read_error_queue :: TChan SomeError -- write only
    ,read_event_queue :: TChan SomeEvent -- write only
    ,read_input_queue :: Handle -- read only
    ,read_reps :: TChan PendedReply -- read only
    ,read_config :: ConnectionConfig
    ,read_extensions :: TVar ExtensionMap
    }

---- Processing for events/errors

-- reverse-lookup infrastructure for extensions.  Not pretty or
-- maybe not even fast. But it is straight-forward.
queryExtMap :: (QueryExtensionReply -> CARD8)
            -> ReadLoop -> CARD8 -> IO (Maybe (ExtensionId, CARD8))
queryExtMap f r code = do
  ext_map <- atomically . readTVar $ read_extensions r
  return $ findFromCode ext_map
 where findFromCode xmap = foldr go Nothing (M.toList xmap)
       go (ident, extInfo) old
           | num <= code =
               case old of
                 Just (_oldIndent, oldNum) |  oldNum > num -> old
                 _ -> Just (ident, num)
           | otherwise = old
         where num = f extInfo

-- | Returns the extension id and the base event code
extensionIdFromEventCode :: ReadLoop -> CARD8
                         -> IO (Maybe (ExtensionId, CARD8))
extensionIdFromEventCode = queryExtMap first_event_QueryExtensionReply

-- | Returns the extension id and the base error code
extensionIdFromErrorCode :: ReadLoop -> CARD8
                         -> IO (Maybe (ExtensionId, CARD8))
extensionIdFromErrorCode = queryExtMap first_error_QueryExtensionReply


bsToError :: ReadLoop
          -> ByteString -- ^Raw data
          -> Word8 -- ^Error code
          -> IO SomeError
bsToError _r chunk code | code < 128 = case deserializeError code of
     Nothing -> return . toError . UnknownError $ chunk
     Just getAction -> return $ runGet getAction chunk
bsToError r chunk code
    = extensionIdFromErrorCode r code >>= \errInfo -> case errInfo of
         Nothing -> return . toError . UnknownError $ chunk
         Just (extId, baseErr) ->
             case errorDispatch extId (code - baseErr) of
               Nothing -> return . toError . UnknownError $ chunk
               Just getAction -> return $ runGet getAction chunk
                 

bsToEvent :: ReadLoop
          -> ByteString -- ^Raw data
          -> Word8 -- ^Event code
          -> IO SomeEvent
bsToEvent _r chunk code | code < 64 = case deserializeEvent code of
    Nothing -> return . toEvent . UnknownEvent $ chunk
    Just getAction -> return $ runGet getAction chunk
bsToEvent r chunk code
    = extensionIdFromEventCode r code >>= \evInfo -> case evInfo of
         Nothing -> return . toEvent . UnknownEvent $ chunk
         Just (extId, baseEv) ->
             case eventDispatch extId (code - baseEv) of
               Nothing -> return . toEvent . UnknownEvent $ chunk
               Just getAction -> return $ runGet getAction chunk

deserializeInReadLoop rl = deserialize

readBytes :: ReadLoop -> Int -> IO ByteString
readBytes rl n = BS.hGet (read_input_queue rl) n

-- the read loop slurps bytes off of the handle, and places
-- them into the appropriate shared structure.
readLoop :: ReadLoop -> IO ()
readLoop rl = do
  chunk <- readBytes rl 32
  let genRep = flip runGet chunk $ deserialize
               
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
  let bytes = chunk `BS.append` extra

  -- place the response into the pending reply TMVar, or discard it
  atomically $ do
    nextPend <- readTChan $ read_reps rl
    if (pended_sequence nextPend) == (grep_sequence genRep)
     then case pended_reply nextPend of
            WrappedReply replyHole -> 
                let reply = flip runGet bytes $ deserializeInReadLoop rl
                in putReceipt replyHole $ Right reply
     else unGetTChan (read_reps rl) nextPend


-- take the bytes making up the error response, shove it in
-- a queue.
--
-- If the error corresponds to one of the pending replies,
-- place the error into the pending reply TMVar instead.
readLoopError rl genRep chunk = do
  let errorCode = grep_error_code genRep

  err <- bsToError rl chunk errorCode
  atomically $ do
    nextPend <- readTChan $ read_reps rl
    if (pended_sequence nextPend) == (grep_sequence genRep)
     then case pended_reply nextPend of
            WrappedReply replyHole -> putReceipt replyHole (Left err)
     else do
       unGetTChan (read_reps rl) nextPend
       writeTChan (read_error_queue rl) err

-- take the bytes making up the event response, shove it in
-- a queue
readLoopEvent rl genRep chunk = do
    ev <- bsToEvent rl chunk eventCode
    atomically $ writeTChan (read_event_queue rl) ev

 where eventCode = case grep_response_type genRep of
                     ResponseTypeEvent w -> w .&. 127

-- | Connect to the the default display.
connect :: IO (Maybe Connection)
connect = connectTo ""

-- | Connect to the display specified.
-- The string must be of the format used in the
-- DISPLAY environment variable.
connectTo :: String -> IO (Maybe Connection)
connectTo display = do
  (h, xau, dispName) <- open display
  hSetBuffering h NoBuffering
  mkConnection h xau dispName

-- | Returns the information about what we originally tried to
-- connect to.
displayInfo :: Connection -> DispName
displayInfo = conn_dispInfo

-- Handshake with the server
-- parse result of handshake
-- launch the thread which holds the handle for reading
mkConnection :: Handle -> Maybe Xauth -> DispName -> IO (Maybe Connection)
mkConnection hnd auth dispInfo = do
  errorQueue <- newTChanIO
  eventQueue <- newTChanIO
  replies <- newTChanIO
  sequence <- initialSequence
  extensions <- newTVarIO mempty

  wrappedHandle <- newMVar hnd 

  confM <- handshake hnd auth
  if isNothing confM then return Nothing else do
  let Just conf = confM

  rIds <- newTVarIO $ resourceIds conf

  let rlData = ReadLoop errorQueue eventQueue hnd replies conf extensions
  
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
      extensions
      dispInfo

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
    deserialize = do
      status <- deserialize
      skip 5
      length <- deserialize
      return $ GenericSetup status length 

data SetupStatus = SetupFailed | SetupAuthenticate | SetupSuccess
 deriving Show

instance Deserialize SetupStatus where
    deserialize = wordToStatus `liftM` deserialize
        where wordToStatus :: Word8 -> SetupStatus
              wordToStatus 0 = SetupFailed
              wordToStatus 1 = SetupSuccess
              wordToStatus 2 = SetupAuthenticate
              wordToStatus n = error $ 
                     "Unkonwn setup status flag: " ++ show n

-- send the setup request to the server,
-- receive the setup response
handshake :: Handle -> Maybe Xauth -> IO (Maybe ConnectionConfig)
handshake hnd auth = do

  -- send setup request
  let requestChunk =  runPut $ serialize $ setupRequest auth
  BS.hPut hnd $ requestChunk

  -- grab an 8-byte chunk to get the response type and size
  firstChunk <- BS.hGet hnd 8

  let genSetup = runGet deserialize firstChunk

  -- grab the rest of the setup response
  secondChunk <- BS.hGet hnd $ fromIntegral $ (4 *) $ setup_length genSetup
  let setupBytes = firstChunk `BS.append` secondChunk

  -- handle the response type
  case setup_status genSetup of
    SetupFailed -> do
       let failed = runGet deserialize setupBytes
           failMessage = map castCCharToChar (reason_SetupFailed failed)
       hPutStrLn stderr failMessage
       return Nothing
    SetupAuthenticate -> do
       let auth = runGet deserialize setupBytes
           authMessage = map castCCharToChar (reason_SetupAuthenticate auth)
       hPutStrLn stderr authMessage
       return Nothing
    SetupSuccess -> do
       let setup = runGet deserialize setupBytes
       return . return $ ConnectionConfig setup
       
padBS n = BS.replicate n 0

initialSequence :: IO (TVar SequenceId)
initialSequence = newTVarIO 1

setupRequest :: Maybe Xauth -> SetupRequest
setupRequest auth = MkSetupRequest
                  (fromIntegral $ byteOrderToNum byteOrder)
                  11       -- major version
                  0        -- minor version
                  anamelen -- auth name length
                  adatalen -- auth data length
                  -- TODO this manual padding is a horrible hack, it should be
                  -- done by the serialization instance
                  (aname ++ replicate (requiredPadding anamelen) 0)
                           -- auth name
                  (adata ++ replicate (requiredPadding adatalen) 0)
                           -- auth data
 where
    (anamelen, aname, adatalen, adata) = case auth of
        Nothing -> (0, [], 0, [])
        Just (Xauth n d) -> (genericLength n, n, genericLength d, d)


-- | I plan on deprecating this one soon, but until I put together
-- some sort of 'utils' package, it will live here.
--
-- Given a connection, this function returns the root window of the
-- first screen.
--
-- If your display string specifies a screen other than the first,
-- this probably doesnt do what you want.
getRoot :: Connection -> WINDOW
getRoot = root_SCREEN . head . roots_Setup . conf_setup . conn_conf
