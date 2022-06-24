module Locks.Tests where

import Control.Concurrent.Async
import Control.Monad (replicateM_)
import Data.IORef
import Locks.Class
import Locks.Spinlock (Spinlock (Spinlock))
import System.CPUTime

--action l state = lock l >> (readIORef state >>= (\x -> writeIORef state $! (x + 1))) >> unlock l
--
--testLock l = do
--  state <- newIORef 0
--  mapConcurrently_ (\x -> replicateM_ 500000 (action l state)) [0 .. 2]
--  readIORef state >>= print
--
--timeItT :: (Show a) => IO a -> IO ()
--timeItT ioa = do
--  t1 <- getCPUTime
--  a <- ioa
--  t2 <- getCPUTime
--  let t :: Double
--      t = fromIntegral (t2 - t1) * 1e-12
--  print $ (t, a)
--
--testLocks = do
--  timeItT $ testSpinlock
--  timeItT $ testTicket
--
--testSpinlock = do
--  (l :: Spinlock) <- new
--  testLock l
--
--testTicket = do
--  (l :: TicketLock1) <- new
--  testLock l