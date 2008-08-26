import Foreign.C.String
import Data.Word

import qualified Data.ByteString.Lazy as BS

import qualified XHB.Connection as X
import qualified XHB.Connection.Open as X
import qualified XHB.Shared as X

import qualified XHB.Gen.Xproto.Types as X
import XHB.Gen.Xproto

import qualified XHB.Gen.Xinerama as Xinerama
import qualified XHB.Gen.Xinerama.Types as Xinerama

import System.IO

main = do
  (h, auth) <- X.open ""

  hSetBuffering h NoBuffering

  connectionM <- X.mkConnection h auth

  case connectionM of
    Nothing -> putStrLn "failed to get connection"
    Just c -> demo c

demo :: X.Connection -> IO ()
demo c = do

  -- send two requests
  listReceipt <- listExtensions c
  ssReceipt <- getScreenSaver c

  -- create a window
  wid <- X.newResource c
  createWindow c $ demoCreateWindowReq c wid
  mapWindow c wid  

  -- process first request
  replyOrError <- X.getReply listReceipt
  case replyOrError of
    Left e -> putStrLn $ "error in extensions request" ++ showError e
    Right listRep -> printExtReply listRep

  -- process second request
  replyOrError <- X.getReply ssReceipt
  case replyOrError of
    Left e -> putStrLn $ "error in screen saver request" ++ showError e
    Right ssRep -> printSSRep ssRep

  isActiveReceipt <- Xinerama.isActive c
  replyOrError <- X.getReply isActiveReceipt
  case replyOrError of
    Left e -> putStrLn $ "error checking if xinerama is active" ++ showError e
    Right isActiveReply -> printIsActive isActiveReply

  putStrLn ""
  putStrLn "Press any key to continue"
  hSetBuffering stdin NoBuffering
  getChar
  putStrLn ""

printIsActive rep | Xinerama.state_IsActiveReply rep == 0 = do
                          putStrLn ""
                          putStrLn "Xinerama is not active"
                  | otherwise = do
                          putStrLn ""
                          putStrLn "Xinerama is active!"

printExtReply :: X.ListExtensionsReply -> IO ()
printExtReply r =
    let names = map strToString (X.names_ListExtensionsReply r)
    in sequence_ $ map putStrLn $ "" : names

printSSRep :: X.GetScreenSaverReply -> IO ()
printSSRep r = sequence_ $ map putStrLn
  [""
  ,"Screen saver info:"
  ," Interval: " ++ show (X.interval_GetScreenSaverReply r)
  ," Timeout: " ++ show (X.timeout_GetScreenSaverReply r)
  ]

-- this could be in a library somewhere
strToString :: X.STR -> String
strToString = map castCCharToChar . X.name_STR

-- | Create the data needed for the 'createWindow' request
demoCreateWindowReq :: X.Connection -> X.WINDOW -> X.CreateWindow
demoCreateWindowReq c w
    = X.MkCreateWindow
      0
      w
      (X.getRoot c)
      0
      0
      100
      100
      5
      0
      0
      (X.emptyValueParam)

-- errors are returned as bytestrings, currently
showError :: X.RawError -> String
showError = show . BS.unpack

