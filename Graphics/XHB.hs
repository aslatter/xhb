-- Module to re-export te portions of XHB.Shared that should be public,
-- as well as tie together core functionality.


{- |

X Haskell Bindings
This module containes (or re-exports) the core functionality needed to
communicate with an X server.

No frills.

If you'd like to work with an extension to the X Protocol, you should be
able to find what you're looking for in one of the Graphics.XHB.Gen.*
modules.  Also, the module Graphics.XHB.Connection.Extension may be of use.

-}

module Graphics.XHB
    ( module Graphics.XHB.Connection
    , module Graphics.XHB.Gen.Xproto
    , Xid
    , XidLike(..)
    , SimpleEnum(..)
    , BitEnum(..)
    , fromMask
    , toMask
    , ValueParam()
    , toValueParam
    , fromValueParam
    , emptyValueParam
    , stringToCList
    , Receipt
    , getReply
    , Error(..)
    , SomeError
    , UnknownError(..)
    , Event(..)
    , SomeEvent
    , UnknownEvent(..)
    , CARD8
    , CARD16
    , CARD32
    , INT8
    , INT16
    , INT32
    , BOOL
    , BYTE
     ) where

import Graphics.XHB.Connection
import Graphics.XHB.Shared

import Graphics.XHB.Gen.Xproto
