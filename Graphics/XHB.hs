-- Module to re-export te portions of XHB.Shared that should be public,
-- as well as tie together core functionality.


module Graphics.XHB
    ( module Graphics.XHB.Connection
    , module Graphics.XHB.Gen.Xproto
    , module Graphics.XHB.Gen.Xproto.Types
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
import Graphics.XHB.Gen.Xproto.Types