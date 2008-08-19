-- helper functions for working with extensions.

module XHB.Connection.Extension where

import Control.Exception(assert)

import Data.Binary(Put)

import Data.List(genericLength)

import XHB.Gen.Xproto
import XHB.Gen.Xproto.Types

import XHB.Connection.Internal
import XHB.Connection.Types
import XHB.Connection
import XHB.Shared

-- | Convert an extension request to a put action.
-- Hanldes grabbing the extension opcode and feeding it
-- into the 'serializeRequest' function.
serializeExtensionRequest :: ExtensionRequest a => Connection -> a -> IO Put
serializeExtensionRequest c req = do
  extRep <- extensionInfo c $ extensionId req
  let present = extensionPresent extRep
      opCode = extensionOpCode extRep

      bo = byteOrderFromConn c
      putAction = serializeRequest req opCode bo

  assert present $ return ()
  return putAction

-- | Lookup an extension.  Will attempt to check the extension cache
-- first.  Will block until the info is retrieved from the server.
-- Will cache the extension information when found.
extensionInfo :: Connection -> ExtensionId -> IO (QueryExtensionReply)
extensionInfo c extId = do
  extInfoMaybe <- lookupExtension c extId
  case extInfoMaybe of
    Just extInfo -> return extInfo
    Nothing -> do
      receipt <- queryExtension c (genericLength extId) (stringToCList extId)
      reply <- getReply receipt
      case reply of
        Left{} -> error "Fatal error resolving extension info"
        Right extInfo -> do
           cacheExtensionInfo c extId extInfo
           return extInfo

-- friendly helper functions.

extensionOpCode :: QueryExtensionReply -> RequestOpCode
extensionOpCode = major_opcode_QueryExtensionReply

extensionPresent :: QueryExtensionReply -> Bool
extensionPresent = (/= 0) . present_QueryExtensionReply
