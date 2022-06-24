module P2Mvar where

import Control.Concurrent
import Control.Exception

modifyMVar' mvar f = mask $ \restore -> do
  val <- takeMVar mvar
  (a, b) <- restore (f val >>= evaluate) `onException` putMVar mvar val
  putMVar mvar a
  return b

--- why evaluate neccessary

t :: IO Int
t = do
  b <- return undefined -- no exception thrown
  return 2

t2 = (return undefined >>= evaluate) >> return 2 -- exception thrown
