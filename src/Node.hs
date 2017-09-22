module Node (
  Node,
  NodeId,
  nodeIO,
  start,
  send,
  recv
) where

import qualified Control.Concurrent.Async as Async
import qualified Data.Map.Strict as Map
import qualified Control.Monad.Trans.State.Lazy as StateT
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM as STM
import qualified Data.Binary as Binary
import qualified Data.Maybe as Maybe
import qualified Control.Concurrent as Conc
import qualified Network.Simple.TCP as TCP
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as BSS
import qualified Message as Message

type IP = String
type Port = String
type NodeId = (IP, Port)
type Tagged a = (NodeId, a)
type StateIO s a = StateT.StateT s IO a

data MetaNode msg = MetaNode {
  writeChans :: Map.Map NodeId (TChan.TChan msg),
  readChan :: TChan.TChan msg,
  tcpServer :: Conc.ThreadId,
  tcpClients :: [Conc.ThreadId],
  nodeId :: NodeId
}

type Node msg a = StateIO (MetaNode msg) a

nodeIO :: IO a -> Node msg a
nodeIO = Trans.lift

send :: Message.Msg msg => NodeId -> msg -> Node msg ()
send nid msg = do
  node <- StateT.get
  case Map.lookup nid $ writeChans node of
    Nothing -> return ()
    Just chan -> Trans.lift $ STM.atomically $ TChan.writeTChan chan msg

recv :: Message.Msg msg => Node msg msg
recv = do
  node <- StateT.get
  received <- Trans.lift $ STM.atomically $ TChan.readTChan $ readChan node
  return received

receiver :: Message.Msg msg => NodeId -> TChan.TChan msg -> IO ()
receiver (host, port) chan = TCP.serve (TCP.Host host) port $ 
  (\(sock, _) -> do
    Monad.forever $ do
      -- THIS MESSAGE MUST BE DECODED
      m <- TCP.recv sock 8192 
      case m of
        Nothing -> return ()
        Just _m -> STM.atomically $ TChan.writeTChan chan $ Binary.decode $ BSS.fromStrict _m)

sender :: Message.Msg msg => (NodeId, TChan.TChan msg) -> IO ()
sender ((host, port), chan) = do
  TCP.connect host port (\(sock, _) -> do
                          Monad.forever $ do
                            msg <- STM.atomically $ TChan.readTChan chan
                            TCP.send sock $ BSS.toStrict $ Binary.encode msg)

start :: Message.Msg m => NodeId -> [NodeId] -> Node m () -> IO ()
start nid nids comp = do
  readChan <- STM.atomically $ TChan.newTChan
  writeChans <- mapM (\n -> do
                        chan <- STM.atomically $ TChan.newTChan
                        return (n, chan))
                     nids
  serverThread <- Conc.forkIO $ receiver nid readChan
  Conc.threadDelay 8000
  clientThreads <- mapM (\chan -> Conc.forkIO $ sender chan) writeChans
  let metaNode = MetaNode {
    writeChans = Map.fromList writeChans,
    readChan = readChan,
    tcpServer = serverThread,
    tcpClients = clientThreads,
    nodeId = nid
  }
  StateT.runStateT comp metaNode
  return ()
  
  

  
