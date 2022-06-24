{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- from atomic-primops package
module DataStructures.ChaseLevDeque where

import Control.Monad
import Data.Atomics
import Data.Atomics.Counter
import Data.Foldable (traverse_)
import Data.IORef
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import GHC.IO

getCirc :: MV.IOVector a -> Int -> IO a
getCirc !arr !ind = MV.read arr (ind `mod` MV.length arr)

putCirc :: MV.IOVector a -> Int -> a -> IO ()
putCirc !arr !ind x = MV.write arr (ind `mod` MV.length arr) x

growCirc :: Int -> Int -> MV.IOVector a -> IO (MV.IOVector a)
growCirc !start !end !oldarr = do
  let len = MV.length oldarr
      elems = end - start
  newarr <- MV.new (len + len)
  forM_ [start .. (end - 1)] $ \ind -> do
    x <- getCirc oldarr ind
    evaluate x
    putCirc newarr ind x
  return $! newarr

data ChaseLevDeque a = CLD
  { top :: {-# UNPACK #-} !AtomicCounter,
    bottom :: {-# UNPACK #-} !AtomicCounter,
    activeArr :: {-# UNPACK #-} !(IORef (MV.IOVector a))
  }

newQ :: IO (ChaseLevDeque elt)
newQ = do
  v <- MV.new 32
  r1 <- newCounter 0
  r2 <- newCounter 0
  r3 <- newIORef v
  return $! CLD r1 r2 r3

growL b t oldArr = do
  arr <- readIORef oldArr
  let (!capacity, !size) = (MV.length arr, b - t)
  if (size >= capacity - 1)
    then do
      arr' <- growCirc t b arr
      writeIORef oldArr arr' -- Only a single thread will do this!:
      return arr'
    else return arr

pushL :: ChaseLevDeque a -> a -> IO ()
pushL CLD {..} obj = do
  b <- readCounter bottom
  t <- readCounter top
  arr <- growL b t activeArr
  putCirc arr b obj
  writeBarrier --  we need to put write barrier here since otherwise we might    end with elem not added to q->elements, but q->bottom already    modified (write reordering) and with stealWSDeque_ failing    later when invoked from another thread since it thinks elem is    there (in case there is just added element in the queue). Thisissue concretely hit me on ARMv7 multi-core CPUs
  writeCounter bottom (b + 1)
  return ()

{-# INLINE tryPopL #-}
tryPopL :: ChaseLevDeque elt -> IO (Maybe elt)
tryPopL CLD {..} = do
  b <- readCounter bottom
  arr <- readIORef activeArr
  b <- evaluate (b - 1)
  writeCounter bottom b
  storeLoadBarrier -- very important that the following read of q->top does not occur before the earlier write to q->bottom.
  tt <- readCounterForCAS top
  let t = peekCTicket tt
  case b - t of
    !size | size < 0 -> writeCounter bottom t >> return Nothing
    size | size > 0 -> getCirc arr b >>= (\x -> return $! Just x)
    size | size == 0 -> do
      obj <- getCirc arr b
      (b, _) <- casCounter top tt (t + 1)
      writeCounter bottom (t + 1)
      if b
        then return $! Just obj
        else return $ Nothing

-- |  Multiple threads may concurrently attempt steals from the same thread.
tryPopR :: ChaseLevDeque elt -> IO (Maybe elt)
tryPopR CLD {..} = do
  tt <- readCounterForCAS top -- NB. these loads must be ordered, otherwise there is a race between steal and pop.
  loadLoadBarrier
  b <- readCounter bottom
  arr <- readIORef activeArr
  let t = peekCTicket tt
  if (b - t) <= 0
    then return Nothing
    else do
      obj <- getCirc arr t
      (b, _) <- casCounter top tt (t + 1)
      if b
        then return $! Just obj
        else return Nothing -- Someone beat us, abort
