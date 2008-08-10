
module XHB.Connection.Open where

import System.Environment(getEnv)
import System.IO

import Control.Exception

import Network.Socket

-- |This probably only works for me, but it's good enough for testing
open :: Maybe String -> IO (Maybe Handle)
open Nothing = do
  envOrErr <- try $ getEnv "DISPLAY"

  case envOrErr of
    Left{} -> return Nothing
    Right displayName ->
       let addr = SockAddrUnix displayName
           family = AF_UNIX
       in do
         s <- socket family Stream defaultProtocol
         connect s addr
         
         h <- socketToHandle s ReadWriteMode

         return $ return $ h
