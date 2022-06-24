{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module DataStructures.TreiberStack where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.ST
import Data.Atomics
import Data.IORef
import Data.Maybe
import GHC.Exts (Int (I#))
import GHC.IO
import GHC.IORef
import GHC.Prim
import GHC.ST
import GHC.STRef
import System.Mem

-- https://mail.haskell.org/pipermail/haskell-cafe/2010-June/079532.html
-- https://www.reddit.com/r/haskell/comments/e1l329/reallyunsafeptrequality_but_safe/
data Stack a = Stack {head :: IORef ([a])}

new = Stack <$> newIORef []

pop s@(Stack h) = do
  n <- readIORef h
  case n of
    [] -> return Nothing
    (a : xs) -> do
      (b, _) <- cas' xs n h
      if b then return (Just a) else pop s

push a s@(Stack h) = do
  x <- readIORef h
  let new = (a : x)
  (b, _) <- cas' new x h
  if b then return True else push a s

push' a s@(Stack h) = readForCAS h >>= go
  where
    go ticket = do
      let new = (a : peekTicket ticket)
      (b, ticket') <- casIORef h ticket new
      if b then return True else go ticket'

pop' s@(Stack h) = readForCAS h >>= go
  where
    go ticket = do
      case peekTicket ticket of
        [] -> return Nothing
        (a : xs) -> do
          (b, ticket') <- casIORef h ticket xs
          if b then return (Just a) else go ticket'

cas new old h = atomicModifyIORef' h (\val -> let !b = ptrEq val old in (if b then new else val, (b, val)))
  where
    {-# NOINLINE ptrEq #-}
    ptrEq :: a -> a -> Bool
    ptrEq !x !y = I# (reallyUnsafePtrEquality# x y) == 1

cas' :: a -> a -> IORef a -> IO (Bool, a)
cas' new old (IORef (STRef var#)) = stToIO $
  ST $ \s1# ->
    case casMutVar# var# old new s1# of
      (# s2, x#, res #) -> (# s2, (I# (x# ==# 0#) == 1, res) #)

size s@(Stack h) = do
  xs <- readIORef h
  return $ length xs

sum' s@(Stack h) = do
  xs <- readIORef h
  return $ sum xs

pushN n arr = mapConcurrently (\x -> replicateM_ 1000000 (push' 1 arr)) [1 .. (n - 1)]

testT = do
  arr <- new
  -- forkIO $ forever (performGC >> threadDelay 10000)
  traverse (\x -> push 2 arr) [(1 :: Int) .. 12000000]
  -- forkIO $ forever $ (size arr) >>= print >> threadDelay 1000000

  (_, r) <- concurrently (pushN 12 arr) (replicateM 1000000 (pop' arr))
  let acq = (sum $ mapMaybe id r)
  left <- (sum' arr)
  print (show acq ++ " " ++ show left ++ " " ++ show (acq + left))

-- >>> testT
-- "4000001000000 500000500000 4500001500000"
--
-- >>> sum [1..3000000]
-- 45000150000
--
