{-# LANGUAGE RecordWildCards #-}

module DataStructures.ArrayBlockingQueue where

import AsyncTMVar (concurrently)
import Control.Concurrent hiding (signalQSem, waitQSem)
import Control.Exception
import Control.Monad (replicateM, replicateM_)
import Data.Aeson.KeyMap (size)
import Data.IORef
import Data.Vector.Mutable qualified as V
import Semaphores
import System.Timeout

-- fix indexed 
-- boring skipping it is wrong

data ArrayBlockingQueueIO e = ArrayBlockingQueueIO
  { iempty :: QSem1,
    ifull :: QSem1,
    ilock :: MVar (),
    ihead :: IORef Int,
    itail :: IORef Int,
    vec :: V.IOVector e
  }

nexIndex ioRef size = do
  item <- readIORef ioRef
  if item + 1 == size then writeIORef ioRef 0 else writeIORef ioRef (item + 1)
  return item

count :: ArrayBlockingQueueIO a -> IO Int
count ArrayBlockingQueueIO {..} = withMVar ilock $ \_ -> do
  h <- readIORef ihead
  t <- readIORef itail
  if t >= h
    then return $ t - h
    else return $ V.length vec + t - h

push' (ArrayBlockingQueueIO {..}) item = mask_ $ do
  flip onException (signalQSem iempty) $ do
    takeMVar ilock -- if exception occurs here restore semaphore
    i <- nexIndex itail (V.length vec)
    V.write vec i item
    putMVar ilock () -- cannot block
  signalQSem ifull

push :: ArrayBlockingQueueIO a -> a -> IO ()
push a@(ArrayBlockingQueueIO {..}) item = mask_ $ waitQSem iempty >> push' a item

pop :: ArrayBlockingQueueIO a -> IO a
pop (ArrayBlockingQueueIO {..}) = mask_ $ do
  b <- tryWaitQSem ifull
  x <- flip onException (signalQSem ifull) $ do
    takeMVar ilock -- if exception occurs here restore semaphore
    i <- nexIndex ihead (V.length vec)
    x <- V.read vec i
    putMVar ilock () -- cannot block
    return x
  signalQSem iempty -- cannot be interrupted
  return x

peek :: ArrayBlockingQueueIO a -> IO a
peek (ArrayBlockingQueueIO {..}) = mask_ $ do
  waitQSem ifull
  flip finally (signalQSem ifull) $ do
    takeMVar ilock -- if exception occurs here restore semaphore
    i <- readIORef ihead
    x <- V.read vec i
    putMVar ilock () -- cannot block
    return x

pushNB a@(ArrayBlockingQueueIO {..}) item = mask_ $ do
  b <- tryWaitQSem iempty
  if b then push' a item >> return True else return False

popNB = undefined

pushTimeout time a item = timeout time (push a item) >>= (return . maybe False (const True))

popTimeout time a = do
  timeout time (pop a)

clear = undefined

contains = undefined

newArr size = do
  e <- new size
  f <- new 0
  lock <- newMVar ()
  head <- newIORef 0
  tail <- newIORef 0
  vector <- V.unsafeNew size
  return $ ArrayBlockingQueueIO e f lock head tail vector

testArrBlockingQueue = do
  arr <- newArr 10
  (_, r) <- concurrently (replicateM_ 100 (push arr 1)) (replicateM 100 (pop arr))
  print (length r)
  count arr >>= print