{-# LANGUAGE RecordWildCards #-}

module Basic.LoggerExSTM where

import Control.Concurrent (ThreadId, forkFinally, forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad
import GHC.Conc (threadStatus)
import Prelude hiding (log)

data Msg = Msg String | Close ()

data Logger = Logger {tid :: ThreadId, chan :: (TMVar Msg), isOpen :: (TVar Bool)}

newLogger :: IO Logger
newLogger = do
  chan <- newEmptyTMVarIO
  isOpen <- newTVarIO True
  tid <- forkFinally (start chan isOpen) (\_ -> atomically $ writeTVar isOpen False)
  return $ Logger tid chan isOpen
  where
    start chan isOpen = do
      join $
        atomically $ do
          msg <- takeTMVar chan
          case msg of
            Msg str -> return (print str >> start chan isOpen)
            Close close -> return (return ())

log (Logger {..}) str = atomically $ do
  b <- readTVar isOpen
  when b $ putTMVar chan (Msg str)

close (Logger {..}) = do
  atomically $ putTMVar chan (Close ())
  atomically $ do
    b <- readTVar isOpen
    when b $ retry

test logger s = replicateM_ 1000 $ do
  threadDelay 1000
  log logger s

test1 :: IO ()
test1 = do
  logger <- newLogger
  forkIO (test logger "a")
  forkIO (test logger "b")
  threadDelay 30000
  close logger
  threadStatus (tid logger) >>= print

-- >>> test1
