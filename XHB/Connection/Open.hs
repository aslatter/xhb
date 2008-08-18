module XHB.Connection.Open (open) where

import System.Environment(getEnv)
import System.IO hiding (openFile)

import Control.Exception
import Control.Monad

import Network.Socket


-- | Open a Handle to the X11 server specified in the argument.  The DISPLAY
-- environment variable is consulted if the argument is null.
open :: String -> IO Handle
open s = do
    name <- if null s then getEnv "DISPLAY" else return s
    Just (family, addr, sn) <- return $ parse name

    s <- socket family Stream defaultProtocol
    connect s addr

    socketToHandle s ReadWriteMode

-- | Parse the contents of an X11 DISPLAY string.
parse :: String -> Maybe ( Family
                         , SockAddr
                         , Int -- ^ screen number
                         )
parse name = do
    (host, displayscreen) <- splitLast ':' name
    (d, s) <- case splitLast '.' displayscreen of
        Just (display, screen) -> liftM2 (,) (readM display) (readM screen)
        Nothing -> fmap (flip (,) 0) (readM displayscreen)

    let file path = (AF_UNIX, SockAddrUnix path, s)

    case () of
        _ | null host        -> return . file $ unix (d :: Int)
          | last host == '/' -> return . file $ name
          | otherwise        -> Nothing -- INET not implemented yet
 where
    unix = ("/tmp/.X11-unix/X"++) . show

-- | Split the list at the last occurence of the given element, returning
-- Nothing if that element does not occur in the list.
splitLast :: Eq a => a -> [a] -> Maybe ([a], [a])
splitLast c s = case break (== c) (reverse s) of
    (ys, x:xs) | x == c -> Just (reverse xs, reverse ys)
    _                   -> Nothing

readM :: (Monad m, Read a) => String -> m a
readM s = case reads s of
    [(x, "")] -> return x
    _         -> fail "readM: failed to parse"
