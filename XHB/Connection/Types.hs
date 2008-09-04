module XHB.Connection.Types where

import XHB.Shared

import Control.Concurrent.STM
import Control.Concurrent

import System.IO

import Data.Word
import Data.Map(Map)

import XHB.Gen.Xproto.Types

data Connection = Connection
    {conn_error_queue :: TChan SomeError -- read only
    ,conn_event_queue :: TChan SomeEvent -- read only
    ,conn_read_loop_tid :: ThreadId
    ,conn_handle :: MVar Handle -- write only
    ,conn_reps :: TChan PendedReply -- insert only
    ,conn_conf :: ConnectionConfig
    ,conn_next_sequence :: TVar SequenceId
    ,conn_resource_ids :: TVar [Xid]
    ,conn_extensions :: TVar (Map ExtensionId QueryExtensionReply)
    }

data ConnectionConfig = ConnectionConfig
    {conf_byteorder :: BO
    ,conf_setup :: Setup
    }

type SequenceId = Word16

data PendedReply = PendedReply
    {pended_sequence :: SequenceId
    ,pended_reply :: WrappedReply
    }

data WrappedReply = forall a . Deserialize a => WrappedReply (Receipt a)
