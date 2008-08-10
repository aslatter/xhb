
module XHB.Connection.Open where

import System.Environment(getEnv)
import System.IO hiding (openFile)

import Control.Exception

import Network.Socket

-- | Opens the standard file-socket on unix for connecting
-- to a local X11 display.
openUnix :: IO Handle
openUnix = openFile "/tmp/.X11-unix/X0"

openFile file = do
  let addr = SockAddrUnix file
      family = AF_UNIX

  s <- socket family Stream defaultProtocol
  connect s addr

  socketToHandle s ReadWriteMode

-- | This works on my mac laptop
openMac :: IO Handle
openMac = do
  file <- getEnv "DISPLAY"

  openFile file