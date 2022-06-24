{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Locks.Spinlock where

import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.Atomics
import Data.IORef
import GHC.Base (casMutVar#, (==#))
import GHC.Exts (Int (I#))
import GHC.IO
import GHC.IORef
import GHC.STRef
import System.Exit
import System.Posix.Signals
import System.Timeout
import Locks.Class

newtype Spinlock = Spinlock {locked :: IORef (Bool)}

lock1 (Spinlock locked) = do
  b <- atomicModifyIORef' locked (\x -> if x then (x, False) else (True, True))
  if b then return () else lock (Spinlock locked)

instance Lock Spinlock where
  new = Spinlock <$> newIORef False

  unlock (Spinlock locked) = atomicWriteIORef locked False

  lock (Spinlock locked) = go
    where
      go = do
        t <- readForCAS locked
        v <- evaluate $ peekTicket t
        loadLoadBarrier
        case v of
          True -> go
          False -> do
            (b, _) <- casIORef locked t (True)
            unless b go
