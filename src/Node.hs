module Node () where

import qualified Control.Concurrent.Async as Async
import qualified Data.Map.Strict as Map
import qualified Control.Monad.Trans.State.Lazy as StateT
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM as STM
import qualified Data.Binary as Binary
import qualified Data.Maybe as Maybe
import qualified Control.Concurrent as Conc
import qualified Network.Socket as Sock

type IP = String
type Port = String
type NodeId = (IP, Port)
type Tagged a = (NodeId, a)
type StateIO s a = StateT.StateT s IO a

data MetaNode msg = MetaNode {
  writeChans :: Map.Map NodeId (TChan.TChan msg),
  readChan :: TChan.TChan msg,
  tcpServer :: Conc.ThreadId
}

type Node msg a = StateIO (MetaNode msg) a

send :: Binary.Binary msg => NodeId -> msg -> Node msg ()
send nid msg = do
  node <- StateT.get
  case Map.lookup nid $ writeChans node of
    Nothing -> return ()
    Just chan -> Trans.lift $ STM.atomically $ TChan.writeTChan chan msg

recv :: Binary.Binary msg => Node msg msg
recv = do
  node <- StateT.get
  received <- Trans.lift $ STM.atomically $ TChan.readTChan $ readChan node
  return received

receiver :: Binary.Binary msg => Sock.Socket -> TChan.TChan msg -> IO ()
receiver socket chan = do
  forever $ do
    m <- Binary.decode (Sock.recv s 8192) :: msg
    atomically $ TChan.writeTChan chan m

sender :: Binary.Binary msg => Sock.Socket -> Map.Map NodeId (TChan.TChan msg) -> IO ()
sender socket chans = do
  forever $ do
    m <- Binary.decode (Sock.recv s 8192) :: msg
    atomically $ TChan.writeTChan chan m

start :: NodeId -> [NodeId] -> IO ()
start nid nids = do
  serverId 
  
  

  
