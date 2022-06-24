{-# LANGUAGE RecordWildCards #-}

module Locks.Ticketlock where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async hiding (wait)
import Control.Exception
import Control.Monad
import Data.Atomics (loadLoadBarrier, storeLoadBarrier, writeBarrier)
import Data.Atomics.Counter (AtomicCounter, incrCounter, newCounter, peekCTicket, readCounter)
import Data.IORef
import Debug.Trace
import GHC.Conc (forkIO)
import Locks.Class

data TicketLock = TicketLock {cur :: !(IORef Int), next :: !(IORef Int), unnecessary :: !(IORef Int)}

{-# NOINLINE readIORefx #-}
readIORefx :: IORef a -> IO a
readIORefx c = storeLoadBarrier >> readIORef c

testTicket = do
  l <- TicketLock <$> newIORef 0 <*> newIORef 0 <*> newIORef 0
  s <- newIORef 0
  mapConcurrently_ (\x -> replicateM_ 500000 (lockunlock l s >> storeLoadBarrier)) [0 .. 3]
  readIORef s >>= print

lockunlock (TicketLock {..}) s = do
  myticket <- atomicModifyIORef' next (\a -> (a + 1, a + 1))
  let wait = do storeLoadBarrier; x <- readIORef cur; x <- evaluate (x + 1); unless (x == myticket) $ wait;
  wait
  atomicModifyIORef' cur (\a -> (a + 1, ())) -- unlock

data BetterTicketLock = BetterTicketLock {curb :: !(AtomicCounter), nextb :: !(AtomicCounter)}

testTicketb = do
  l <- BetterTicketLock <$> newCounter 1 <*> newCounter 0
  s <- newIORef 0

  mapConcurrently_ (\x -> replicateM_ 500000 (lockunlockb l s)) [0 .. 3]
  readIORef s >>= print

lockunlockb (BetterTicketLock {..}) s = do
  myticket <- incrCounter 1 nextb -- it returns new value not old value
  let wait = do
        x <- readCounter curb -- I am not even using readCounter which is similar to readIOref
        unless (x == myticket) $ wait
  wait
  modifyIORef' s (+ 1)
  void $ incrCounter 1 curb
