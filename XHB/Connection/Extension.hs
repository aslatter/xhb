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

-- | convert an extension request to a put action
serializeExtensionRequest :: ExtensionRequest a => Connection -> a -> IO Put
serializeExtensionRequest c req = do
  extRep <- extensionInfo c $ extensionId req
  let present = extensionPresent extRep
      opCode = extensionOpCode extRep

      bo = byteOrderFromConn c
      putAction = serializeRequest req opCode bo

  assert present $ return ()
  return putAction

extensionOpCode :: QueryExtensionReply -> RequestOpCode
extensionOpCode = major_opcode_QueryExtensionReply

extensionPresent :: QueryExtensionReply -> Bool
extensionPresent = (/= 0) . present_QueryExtensionReply

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

