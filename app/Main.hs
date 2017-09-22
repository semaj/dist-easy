{-# LANGUAGE DeriveGeneric #-}
import qualified Node as Node
import qualified Data.List.Split as Split
import qualified Message as Message
import qualified Control.Monad as Monad
import qualified Control.Concurrent as Conc
import qualified Data.Binary as Binary
import System.Environment
import GHC.Generics (Generic)

data M = M String deriving (Generic, Show)
instance Binary.Binary M
instance Message.Msg M

go :: Node.NodeId -> [Node.NodeId] -> Node.Node M ()
go nid nids = do
  Monad.mapM (\x -> Node.send x (M ("from " ++ (show nid) ++ ", hello " ++ (show x)))) nids
  message <- Node.recv 
  Node.nodeIO $ putStrLn $ show message
  Node.nodeIO $ Conc.threadDelay 1000
  go nid nids

main :: IO ()
main = do
  raw <- getArgs
  let args = fmap (\x -> 
                    let s = Split.splitOn ":" x in
                    ((s!!0), (s!!1)))
                  raw
  Node.start (head args) (drop 1 args) (go (head args) (drop 1 args))
  
