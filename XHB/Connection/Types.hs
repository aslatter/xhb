module XHB.Connection.Types where

import XHB.Shared

import Control.Concurrent.STM
import Control.Concurrent

import System.IO

import Data.Word

import XHB.Gen.Xproto.Types

data Connection = Connection
    {conn_error_queue :: TChan RawError -- read only
    ,conn_event_queue :: TChan RawEvent -- read only
    ,conn_read_loop_tid :: ThreadId
    ,conn_handle :: MVar Handle -- write only
    ,conn_reps :: TChan PendedReply -- insert only
    ,conn_conf :: ConnectionConfig
    ,conn_next_sequence :: TVar SequenceId
    ,conn_resource_ids :: TVar [Xid]
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


