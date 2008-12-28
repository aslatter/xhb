import Foreign.C.String
import Data.Word

import qualified Data.ByteString.Lazy as BS

import Control.Concurrent
import Control.Monad
import Data.Maybe

import qualified Graphics.XHB.Connection as X
import qualified Graphics.XHB.Connection.Open as X
import qualified Graphics.XHB.Shared as X

import qualified Graphics.XHB.Gen.Xproto.Types as X
import Graphics.XHB.Gen.Xproto

import qualified Graphics.XHB.Gen.Xinerama as Xinerama
import qualified Graphics.XHB.Gen.Xinerama.Types as Xinerama

import System.IO

main = do
  connectionM <- X.connect

  case connectionM of
    Nothing -> putStrLn "failed to get connection"
    Just c -> demo c

demo :: X.Connection -> IO ()
demo c = do

  handleError c
  handleEvent c

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
      (X.toValueParam [(X.CWEventMask,X.toMask
                             [ X.EventMaskEnterWindow
                             , X.EventMaskLeaveWindow
                             , X.EventMaskFocusChange
                             ]
                       )])

-- print errors from the queue
handleError :: X.Connection -> IO ()
handleError c = do
  forkIO $ forever $ do
      e <- X.waitForError c

      putStrLn $ showError e
  return ()

-- print events from the queue
handleEvent ::X.Connection -> IO ()
handleEvent c = do
  forkIO $ forever $ do
      e <- X.waitForEvent c

      putStrLn $ showEvent e
  return ()

showError :: X.SomeError -> String
showError serr = tryErrors hs showErrorBase serr
 where hs = [ErrorHandler showWindowError
            ,ErrorHandler showUnknownError
            ]

-- show an error of type Window
showWindowError :: X.Window -> String
showWindowError err =
    let badwindow = X.bad_value_Window err
    in "WindowError: bad window: " ++ show badwindow

-- show an UnknownError
showUnknownError :: X.UnknownError -> String
showUnknownError (X.UnknownError bs) = 
   "UnknownError: " ++ (show . BS.unpack $ bs)

-- default case
showErrorBase :: String
showErrorBase = "Unhandled Error"


data ErrorHandler b = forall a . X.Error a => ErrorHandler (a -> b)

tryErrors :: [ErrorHandler b] -> b -> X.SomeError -> b
tryErrors hs z err = case foldr tryErr Nothing hs of
       Nothing -> z
       Just x -> x
 where tryErr _ j@Just{} = j
       tryErr h Nothing = case h of
          ErrorHandler fn -> do
            err' <- X.fromError err
            return $ fn err'




showEvent :: X.SomeEvent -> String
showEvent = tryEvents hs showEventBase

 where hs = [EventHandler showMotionNotify
            ,EventHandler showEnterNotify
            ,EventHandler showLeaveNotify
            ,EventHandler showFocusIn
            ,EventHandler showFocusOut
            ]

showMotionNotify :: X.MotionNotify -> String
showMotionNotify _ = "MotionNotify"

showEnterNotify :: X.EnterNotify -> String
showEnterNotify _ = "EnterNotify" 

showLeaveNotify :: X.LeaveNotify -> String
showLeaveNotify _ = "LeaveNotify"

showFocusIn :: X.FocusIn -> String
showFocusIn _ = "FocusIn"

showFocusOut :: X.FocusOut -> String
showFocusOut _ = "FocusOut"

showEventBase = "unhandled event"


data EventHandler b = forall a . X.Event a => EventHandler (a -> b)


tryEvents :: [EventHandler b] -> b -> X.SomeEvent -> b
tryEvents hs z ev = case foldr tryEv Nothing hs of
        Nothing -> z
        Just x -> x
 where tryEv _ j@Just{} = j
       tryEv h Nothing = case h of
          EventHandler fn -> do
            ev' <- X.fromEvent ev
            return $ fn ev'

