module Graphics.XHB.Connection.Auth (getAuthInfo) where

import Data.Bits
import Data.Word

import System.IO

import Graphics.X11.Xauth

import Network.Socket
import Network.BSD (getHostName)

import Foreign.C (CChar)

-- | Yields libxau record for given socket/display configuration.
getAuthInfo :: Socket -> Int -> IO (Maybe Xauth)
getAuthInfo fd display = do
    sock <- getPeerName fd
    (addr, fam) <- f sock
    getAuthByAddr fam addr (cstring $ show display) (cstring atype)
    where
        f x | isLocal x = getHostName >>= \h ->
                            return (cstring h, 256) -- familyLocal
            -- XCB_FAMILY_INTERNET
            | isIPv4 x || isIPv6Mappedv4 x = return (host x, 0)
            -- XCB_FAMILY_INTERNET_6
            | otherwise = return (host x, 6)

        isLocal (SockAddrUnix _) = True
        isLocal (SockAddrInet _ h) = h == 16777343 -- 127.0.0.1
        isLocal (SockAddrInet6 _ _ (0,0,0,1) _) = True

        isIPv4 (SockAddrInet _ _) = True
        isIPv4 _ = False

        isIPv6Mappedv4 (SockAddrInet6 _ _ (0,0,0xFFFF,x) _) = True
        isIPv6Mappedv4 _ = False

        -- tear it to bytes
        -- we do this because we need bare bytes for comparison on C side
        bytes :: Word32 -> [CChar]
        bytes x = foldr g [] [0,8..24] where
            g a = let r = (x `shiftR` a) .&. 0xFF
                  in ((fromIntegral r):)

        -- N.B.: no endianness conversion necessary
        host (SockAddrInet _ h) = bytes h
        host (SockAddrInet6 _ _ (x,y,z,w) _) = concatMap bytes [x,y,z,w]

        atype = "MIT-MAGIC-COOKIE-1"

cstring :: String -> [CChar]
cstring = map (fromIntegral . fromEnum)

