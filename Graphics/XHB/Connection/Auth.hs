module Module.Graphics.XHB.Connection.Auth (getAuthInfo) where

import Graphics.X11.Xauth

import Network.Socket
import Network.BSD (getHostName)

import Foreign.C (CChar)

-- TODO:
-- * make it work
-- see xcb_auth.c:_xcb_auth_info
getAuthInfo :: Handle -> Int -> IO (Maybe Xauth)
getAuthInfo fd display = do
    sock <- getPeerName fd
    (addr, fam) = f sock
    getAuthByAddr fam (cstring addr) (cstring . show display) []
    where f (SockAddrInet _ h) = undefined
          f (SockAddrInet6 _ _ h _) = undefined
          f (SockAddrUnix String) = undefined

    -- if socket is unix or if it's IP and points to localhost,
    --   then
    --       (getHostName, familyLocal)
    --   else
    --       we use socket address as-is
    --         FIXME: in C, we would get bare bytes, but in Haskell, we can't
    --           IPv4: hostAddress should be turned to raw byte string
    --           IPv6: if v4-mapped
    --                 then same procedure as with IPv4
    --                 else yyyy
    --       family = either IPv4 or IPv6 (special constants here)

cstring :: String -> [CChar]
cstring = map (fromIntegral . fromEnum)

