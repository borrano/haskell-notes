module Locks.Class where

class Lock a where
  new :: IO a
  lock :: a -> IO ()
  unlock :: a -> IO ()