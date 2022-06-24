{-# LANGUAGE RecordWildCards #-}

module Basic.LoggerExMVar where

import Control.Concurrent (ThreadId, forkFinally, forkIO, threadDelay, MVar)
import Control.Concurrent.STM
import Control.Monad
import GHC.Conc (threadStatus)
import Prelude hiding (log)

data Msg = Msg String | Close ()

data Logger = Logger {chan :: (MVar Msg) }

newLogger :: IO Logger
newLogger = undefined

log (Logger {..}) str = undefined

close (Logger {..}) = undefined

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

-- >>> test1
