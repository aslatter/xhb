module Graphics.XHB.Connection.Open (open, DispName(..)) where

import System.Environment(getEnv)
import System.IO

import Control.Exception hiding (try)
import Control.Monad
import Control.Applicative((<$>))

import Data.Foldable (foldrM)

import Network.Socket
import Graphics.X11.Xauth

import Data.Maybe (fromMaybe)
import Text.ParserCombinators.Parsec

import Graphics.XHB.Connection.Auth

data DispName = DispName { proto :: String
                         , host :: String
                         , display :: Int
                         , screen :: Int
                         } deriving Show

-- | Open a Handle to the X11 server specified in the argument.  The DISPLAY
-- environment variable is consulted if the argument is null.
open :: String -> IO (Handle , Maybe Xauth, DispName)
open [] = (getEnv "DISPLAY") >>= open
open disp
    | take 11 disp == "/tmp/launch" = do
        fd <- fromMaybe (error "couldn't open socket") <$> openUnix "" disp
        hndl <- socketToHandle fd ReadWriteMode
        return (hndl, Nothing, launchDDisplayInfo disp)
open xs = let
    cont (DispName p h d s)
        | null h || null p && h == "unix" = openUnix p
             ("/tmp/.X11-unix/X" ++ show d)
        | otherwise = openTCP p h (6000 + d)

    openTCP proto host port
        | proto == [] || proto == "tcp" =
            let addrInfo = defaultHints { addrFlags  = [ AI_ADDRCONFIG
                                                       , AI_NUMERICSERV
                                                       ]
                                        , addrFamily = AF_UNSPEC
                                        , addrSocketType = Stream
                                        }
                conn (AddrInfo _ fam socktype proto addr _) Nothing = do
                   fd <- socket fam socktype proto
                   connect fd addr
                   return $ Just fd
                conn _ x = return x
            in getAddrInfo (Just addrInfo) (Just host) (Just (show port))
               >>= foldrM conn Nothing
        | otherwise = error "'protocol' should be empty or 'tcp'"
 
    in case parseDisplay xs of
        (Left e) -> error (show e)
        (Right x) -> do
             socket <- cont x >>= return . fromMaybe
                 (error "couldn't open socket")
             auth <- getAuthInfo socket (display x) 
             hndl <- socketToHandle socket ReadWriteMode
             return (hndl, auth, x)

openUnix proto file
    | proto == [] || proto == "unix" = do
        fd <- socket AF_UNIX Stream defaultProtocol
        connect fd (SockAddrUnix file)
        return $ Just fd
    | otherwise = error "'protocol' should be empty or 'unix'"


-- | Parse the contents of an X11 DISPLAY environment variable.
-- TODO: make a public version (see xcb_parse_display)
parseDisplay :: String -> Either ParseError DispName
parseDisplay [] = Right defaultDisplayInfo
parseDisplay xs = parse exp "" xs where
    exp = do
        p <- option "" (try $ skip '/') <?> "protocol"
        h <- option "" ((try ipv6) <|> (try host)) <?> "host"
        d <- char ':' >> integer <?> "display"
        s <- option 0 (char '.' >> integer <?> "screen")
        return $ DispName p h d s
    eat c s = char c >> return s
    anyExcept c = many1 (noneOf [c])
    skip c = anyExcept c >>= eat c
    ipv6 = char '[' >> skip ']'
    host = anyExcept ':'
    integer :: Parser Int
    integer = many1 digit >>= \x -> return $ read x

-- | Given a launchd display-string, return the appropriate
-- DispName structure for it.
launchDDisplayInfo :: String -> DispName
launchDDisplayInfo str = case parseDisplay (dropWhile (/= ':') str) of
                    Left{} -> defaultDisplayInfo
                    Right d -> d

defaultDisplayInfo = DispName "" "" 0 0