{-# LANGUAGE ScopedTypeVariables #-}

module Basic.Basic1 where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Map  as M
import Data.Map.Internal.Debug (node)
import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import System.IO
import Prelude hiding (log)

-- handle is wrapped by mvar therefore they take turns printing unless any of them are preempted

ex1 :: IO ()
ex1 = do
  hSetBuffering stdout NoBuffering
  forkIO $ replicateM_ 10 $ putChar 'a'
  replicateM_ 10 $ putChar 'b'

-- >>> ex1
-- bbababababababababaa

-- return type is polymorphic - means it will run forever - cannot produce a polymorphic value
ex2 :: IO b
ex2 = forever $ do
  time <- fmap read getLine
  forkIO $ do
    putStrLn $ "alarm is set to" ++ show time
    threadDelay (1000000 * time)
    putStrLn "alarm \BEL"

-- the program terminates when main returns, even if there are other threads still running.

-- comm between threads - not safe async exceptions
ex3 :: IO ()
ex3 = do
  mvar <- newEmptyMVar
  forkIO $ putMVar mvar "boran" >> putMVar mvar "duygus"
  takeMVar mvar >>= putStrLn
  putStrLn "second value"
  takeMVar mvar >>= putStrLn

-- >>> ex3
-- boran
-- second value
-- duygus
--

--
-- blocks indefinetely
ex4 :: IO b
ex4 = newEmptyMVar >>= takeMVar

-- >>> ex4

-- *** Exception: thread blocked indefinitely in an MVar operation

--

---- mvar 1: channel with one capacity
-- when closed all threads waiting are blocked
-- LoggerExSTM implements this correctly
data Msg = Msg String | Close (MVar ())

newtype Logger = Logger (MVar Msg)

newLogger :: IO Logger
newLogger = do
  mvar <- newEmptyMVar
  forkIO (start mvar)
  return $ Logger mvar
  where
    start mvar = do
      msg <- takeMVar mvar
      case msg of
        Msg str -> print str >> start mvar
        Close close -> print "closing" >> putMVar close ()

log (Logger l) str = putMVar l (Msg str)

close (Logger l) = do
  e <- newEmptyMVar
  putMVar l (Close e)
  takeMVar e

ex5 = do
  l <- newLogger
  log l "boran"
  log l "duygus"
  close l

-- >>> ex5
-- "boran"
-- "duygus"
-- "closing"
--

--- mvar 2: mutable shared state

type Table = MVar (M.Map String Int)

mkTable = newMVar M.empty

-- evaluate thunk to avoid space leaks
insert k v state = mask_ $ do
  map <- takeMVar state
  let m = M.insert k v map
  putMVar state m
  seq m (return ())

--  This is possible only because the value of the state is immutable. If the data structure were mutable, we would have to hold the lock while operating on it
lookup k state = M.lookup k <$> readMVar state

---------------------------------------------------------------------
