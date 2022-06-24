module DataStructures.MichaelScottQueue where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Data.Atomics
import Data.IORef
import Data.Maybe (mapMaybe)
import Debug.Trace

data Queue a = Queue
  { head :: IORef (Node a),
    tail :: IORef (Node a)
  }

data Node a = Node a (IORef (Node a)) | E

newNode a = Node a <$> newIORef E

create = do
  sentinel <- newNode undefined
  Queue <$> newIORef sentinel <*> newIORef sentinel

push (Queue head tail) item = newNode item >>= go
  where
    go newNode = do
      tailTicket <- readForCAS tail
      let (Node _ next) = peekTicket tailTicket
      nextTicket <- readForCAS next
      case peekTicket nextTicket of
        E -> do
          (ok, _) <- casIORef next nextTicket newNode
          if ok then casIORef tail tailTicket newNode >> return () else go newNode
        n -> casIORef tail tailTicket n >> go newNode

pop q@(Queue head tail) = do
  headTicket <- readForCAS head
  let (Node _ next) = peekTicket headTicket
  nextTicket <- readForCAS next
  case peekTicket nextTicket of
    E -> return Nothing
    (Node a _) -> do
      tailTicket <- readForCAS tail
      when (headTicket == tailTicket) $ void $ casIORef2 tail tailTicket nextTicket
      (b, _) <- casIORef2 head headTicket nextTicket
      if b then return (Just a) else pop q

popAll arr = do
  x <- pop arr
  case x of
    Nothing -> return []
    (Just a) -> (a :) <$> popAll arr

pushN n arr = mapConcurrently_ (\x -> replicateM_ mil (push arr (x + 1))) [0 .. (n - 1)]

popN n arr = concat <$> mapConcurrently (\x -> replicateM mil (pop arr)) [0 .. (n - 1)]

test1 = do
  arr <- create
  pushN 1 arr
  left <- sum <$> (popAll arr)
  print left

-- >>> testT
-- "12000 78000 90000"
--

mil = 1000000

testT = do
  arr <- create
  --traverse (\x -> push arr 2) [(1 :: Int) .. (12 * mil)]
  (_, r) <- concurrently (pushN 12 arr) ((popN 12 arr))
  let acq = (sum $ mapMaybe id r)
  left <- sum <$> (popAll arr)
  print (show acq ++ " " ++ show left ++ " " ++ show (acq + left))
