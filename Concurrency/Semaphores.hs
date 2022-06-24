{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
-- https://github.com/simonmar/sem

module Semaphores where

import Control.Concurrent hiding (signalQSem, waitQSem)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM, join, unless, when)
import Data.IORef
import Data.Maybe
import Test.HUnit

class NBSem a where
  new :: Int -> IO a
  tryWaitQSem :: a -> IO Bool
  signalQSem :: a -> IO ()

class (NBSem a) => Sem a where
  waitQSem :: a -> IO ()

newtype NBSemMVar = NBSemMVar (MVar Int)

instance NBSem NBSemMVar where
  new a = NBSemMVar <$> newMVar a
  tryWaitQSem (NBSemMVar a) = modifyMVar a (\a -> return $ if a == 0 then (0, False) else (a - 1, True))
  signalQSem (NBSemMVar a) = modifyMVar_ a (return . (+ 1))

newtype NBSemIORef = NBSemIORef (IORef Int)

instance NBSem NBSemIORef where
  new a = NBSemIORef <$> newIORef a
  tryWaitQSem (NBSemIORef a) = atomicModifyIORef a (\a -> if a == 0 then (0, False) else (a - 1, True))
  signalQSem (NBSemIORef a) = atomicModifyIORef' a (\a -> (a + 1, ()))

newtype QSem1 = QSem1 (MVar (Int, [MVar ()]))

instance NBSem QSem1 where
  new a = QSem1 <$> newMVar (a, [])
  tryWaitQSem (QSem1 x) = mask_ $ do
    (i, xs) <- takeMVar x
    if i /= 0
      then putMVar x (i - 1, xs) >> return True
      else return False

  signalQSem = signalQSem1

instance Sem QSem1 where
  waitQSem = waitQSem1

waitQSem1 (QSem1 x) = mask_ $ do
  (i, xs) <- takeMVar x
  if i /= 0
    then putMVar x (i - 1, xs)
    else do
      mymvar <- newEmptyMVar
      putMVar x (0, xs ++ [mymvar])
      takeMVar mymvar `onException` go mymvar
  where
    go mymvar = uninterruptibleMask_ $ do
      (i, xs) <- takeMVar x
      r <- tryTakeMVar mymvar
      case r of
        Nothing -> putMVar mymvar () >> putMVar x (i, xs)
        (Just _) -> signal (i, xs) >>= putMVar x -- already processed

signalQSem1 (QSem1 m) =
  uninterruptibleMask_ $ do
    r <- takeMVar m
    r' <- signal r
    putMVar m r'

-- signal is called only with uninterruptibleMask and lock held
signal (0, []) = return (1, [])
signal (0, x : xs) = do
  r <- tryPutMVar x ()
  if r then return (0, xs) else signal (0, xs)
signal (i, xs) = return (i + 1, xs)

----------------------------------------

newtype SemSTM = SemSTM (TVar Int)

instance NBSem SemSTM where
  new c = SemSTM <$> newTVarIO c
  tryWaitQSem = undefined
  signalQSem = signalQSem2

instance Sem SemSTM where
  waitQSem = waitQSem2

waitQSem2 (SemSTM tvar) =
  atomically $ do
    c <- readTVar tvar
    when (c == 0) retry
    writeTVar tvar (c - 1)

signalQSem2 (SemSTM tvar) = atomically $ modifyTVar tvar (+ 1)

-----------------------------------------
data QSem3 = QSem3 (TVar Int) (TVar [TVar Bool])

instance NBSem QSem3 where
  new c = QSem3 <$> newTVarIO c <*> newTVarIO []
  tryWaitQSem = undefined
  signalQSem = atomically . signalQSem3

instance Sem QSem3 where
  waitQSem = waitQSem3

waitQSem3 sem@(QSem3 count queue) = mask_ (go >>= maybe (return ()) (\mytvar -> wait mytvar `onException` handle mytvar))
  where
    go = atomically $ do
      i <- readTVar count
      if i /= 0
        then writeTVar count (i - 1) >> return Nothing
        else do
          mytvar <- newTVar False
          modifyTVar queue (++ [mytvar])
          return $ Just mytvar
    wait mytvar = atomically $ do
      b <- readTVar mytvar
      unless b retry
    handle mytvar = atomically $ do
      b <- readTVar mytvar
      if b then signalQSem3 sem else writeTVar mytvar True

signalQSem3 (QSem3 count queue) = do
  c <- readTVar count
  if c /= 0
    then writeTVar count (c + 1)
    else do
      xs <- readTVar queue
      loop xs
  where
    loop [] = writeTVar count 1
    loop (x : xs) = do
      b <- readTVar x
      if b then loop xs else writeTVar x True >> writeTVar queue xs

semTests =
  TestList
    [ "sem1" ~: sem1,
      "sem2" ~: sem2,
      "sem_kill" ~: sem_kill -- ,
      -- "sem_fifo" ~: sem_fifo,
      -- "sem_bracket" ~: sem_bracket
    ]

sem1 = do
  (q :: QSem3) <- new 0
  signalQSem q
  waitQSem q

sem2 = do
  (q :: QSem3) <- new 0
  signalQSem q
  signalQSem q
  waitQSem q
  waitQSem q

sem_fifo = do
  c <- newChan
  (q :: QSem3) <- new 0
  t1 <- forkIO $ do waitQSem q; writeChan c 'a'
  threadDelay 10000
  t2 <- forkIO $ do waitQSem q; writeChan c 'b'
  threadDelay 10000
  t3 <- forkIO $ do waitQSem q; writeChan c 'c'
  threadDelay 10000
  signalQSem q
  a <- readChan c
  signalQSem q
  b <- readChan c
  signalQSem q
  c <- readChan c
  [a, b, c] @?= "abc"

sem_kill = do
  (q :: QSem3) <- new 0
  t <- forkIO $ do signalQSem q
  threadDelay 100000
  killThread t
  m <- newEmptyMVar
  t <- forkIO $ do signalQSem q; putMVar m ()
  waitQSem q
  takeMVar m

sem_bracket = do
  (q :: QSem3) <- new 1
  ts <- forM [1 .. 100000] $ \n -> do
    forkIO $ do bracket_ (signalQSem q) (waitQSem q) (return ())
  mapM_ killThread ts
  signalQSem q