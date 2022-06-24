module P2Chan where

import Control.Concurrent
import Control.Exception

data Node a = Node a (MVar (Node a))

data Chan a = Chan {start :: {-# UNPACK #-} !(MVar (MVar (Node a))), end :: {-# UNPACK #-} !(MVar (MVar (Node a)))}

makeChan = do
  node <- newEmptyMVar
  Chan <$> newMVar node <*> newMVar node

pop (Chan start end) = modifyMVar start $ \node -> do
  (Node item next) <- readMVar node
  return (next, item)

-- The reason we don't simply do this:
--
--    modifyMVar_ writeVar $ \old_hole -> do
--      putMVar old_hole (ChItem val new_hole)
--      return new_hole
--
-- is because if an asynchronous exception is received after the 'putMVar'
-- completes and before modifyMVar_ installs the new value, it will set the
-- Chan's write end to a filled hole.

push elem (Chan start end) = mask_ $ do
  node <- takeMVar end
  newNext <- newEmptyMVar
  putMVar node $ Node elem newNext
  putMVar end newNext

-- >>> t

dupChan (Chan start end) = do
  node <- takeMVar end
  newStart <- newMVar node
  return $ Chan newStart end

unGetChan' :: a -> P2Chan.Chan a -> IO ()
unGetChan' val (Chan start end) = do
  node <- takeMVar start
  newEnd <- newMVar $ Node val node
  putMVar start newEnd

--- deadlock example

deadlock :: IO ()
deadlock = do
  c <- makeChan
  forkIO $ pop c >>= print
  threadDelay (1000000)
  unGetChan' 11 c

-- >>> deadlock
-- *** Exception: thread blocked indefinitely in an MVar operation
--
