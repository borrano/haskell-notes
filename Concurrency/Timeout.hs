{-# LANGUAGE ScopedTypeVariables #-}

module Timeout where

import Control.Concurrent (killThread, myThreadId, threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Data.Data
import Data.Fixed (Uni)
import Data.Unique (Unique, newUnique)
import GHC.Conc (forkIO)

newtype Timeout = Timeout Unique deriving (Typeable, Eq)

instance Show Timeout where
  show _ = "timeout exception"

instance Exception Timeout

timeout1 io = do
  pid <- myThreadId
  u <- newUnique
  let e = Timeout u
  handleJust (\(e' :: Timeout) -> if e == e then Just () else Nothing) (\_ -> return Nothing) $
    bracket (forkIO $ threadDelay 1000000 >> throwTo pid e) (killThread) (\a -> Just <$> io)

timeout2 time io = either (const Nothing) Just <$> race io (threadDelay (time * 1000000))