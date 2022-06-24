{-# LANGUAGE RecordWildCards #-}

module DataStructures.ArrayBlockingQueueSTM where

import AsyncTMVar (concurrently)
import Control.Concurrent hiding (signalQSem, waitQSem)
import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, retry, writeTVar)
import Control.Exception
import Control.Monad (replicateM, replicateM_)
import Data.IORef
import Data.Vector qualified as V
import Semaphores
import System.Timeout

data ArrayBlockingQueueSTM e = ArrayBlockingQueueSTM
  { ihead :: TVar Int,
    itail :: TVar Int,
    vec :: V.Vector (TVar e)
  }

newArr def size = atomically $ do
  head <- newTVar 0
  tail <- newTVar 0
  vector <- V.replicateM (size + 1) (newTVar def)
  return $ ArrayBlockingQueueSTM head tail vector

size ArrayBlockingQueueSTM {..} = V.length vec - 1

count :: ArrayBlockingQueueSTM a -> STM Int
count q@ArrayBlockingQueueSTM {..} = do
  h <- readTVar ihead
  t <- readTVar itail
  if t >= h
    then return $ t - h
    else return $ size q + 1 + t - h

nexIndex tvar q = do
  item <- readTVar tvar
  if item == size q then writeTVar tvar 0 else writeTVar tvar (item + 1)
  return item

push' q@(ArrayBlockingQueueSTM {..}) item = do
  x <- (vec V.!) <$> nexIndex itail q
  writeTVar x item

push :: ArrayBlockingQueueSTM a -> a -> STM ()
push q@(ArrayBlockingQueueSTM {..}) item = do
  c <- count q
  if c == size q
    then retry
    else push' q item

pop' = undefined

pop :: ArrayBlockingQueueSTM a -> STM a
pop q@(ArrayBlockingQueueSTM {..}) = do
  c <- count q
  if c == 0
    then retry
    else pop' q

peek :: ArrayBlockingQueueSTM a -> STM (Maybe a)
peek q@(ArrayBlockingQueueSTM {..}) = do
  c <- count q
  if c == 0
    then return Nothing
    else pop' q

-- pushNB a@(ArrayBlockingQueueIO {..}) item = mask_ $ do
--  b <- tryWaitQSem iempty
--  if b then push' a item >> return True else return False
--
-- popNB = undefined
--
-- pushTimeout time a item = timeout time (push a item) >>= (return . maybe False (const True))
--
-- popTimeout time a = do
--  timeout time (pop a)
--
-- clear = undefined
--
-- contains = undefined
--
-- testArrBlockingQueue = do
--  arr <- newArr 10
--  (_, r) <- concurrently (replicateM_ 100 (push arr 1)) (replicateM 100 (pop arr))
--  print (length r)
--  count arr >>= print