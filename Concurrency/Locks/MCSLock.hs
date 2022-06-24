module Locks.MCSLock where

import Control.Concurrent
import Control.Monad
import Data.Atomics (peekTicket, readForCAS)
import Data.IORef

data Node
  = Node
      { locked :: IORef Bool,
        next :: IORef (Node)
      }
  | E

data MCS = MCS
  { tail :: IORef (Node)
  }

new = MCS <$> newIORef E

lock :: MCS -> IO (Node)
lock (MCS tail) = do
  node <- Node <$> newIORef True <*> (newIORef (E))
  prev <- atomicModifyIORef' tail (\x -> (node, x))
  case prev of
    E -> return node
    (Node _ next) -> do
      writeIORef next node
      let wait = do
            b <- readIORef (locked node)
            when b $ yield >> wait
      wait
      return node
 
unlock (MCS tail) (Node _ next) = do
  nextNode <- readIORef next
  case nextNode of
    E -> undefined