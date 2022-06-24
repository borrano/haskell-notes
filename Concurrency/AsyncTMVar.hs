{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AsyncTMVar where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, catch, mask, try)
import qualified Data.ByteString as B8
import Network.HTTP.Client
import Network.HTTP.Simple

data Async a = Async (TMVar (Either SomeException a)) ThreadId

forkFinally' io after = mask $ \unmask -> forkIO (try (unmask io) >>= after)

async f = do
  tmvar <- newEmptyTMVarIO
  tid <- forkFinally' f (atomically . putTMVar tmvar)
  return $ Async tmvar tid

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async a _) = readTMVar a

waitCatch = atomically . waitCatchSTM

waitSTM :: Async b -> STM b
waitSTM (Async a _) = readTMVar a >>= either throwSTM pure

wait = atomically . waitSTM

waitEitherSTM :: Async a -> Async b -> STM (Either a b)
waitEitherSTM a b = (Left <$> waitSTM a) `orElse` (Right <$> waitSTM b)

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $ waitEitherSTM a b

waitBothSTM :: Async a -> Async b -> STM (a, b)
waitBothSTM a b = do
  a' <- waitSTM a `orElse` (waitSTM b >> retry) -- register both
  b' <- waitSTM b
  return (a', b')

waitBoth :: Async a -> Async b -> IO (a, b)
waitBoth a b = atomically $ waitBothSTM a b

waitAny xs = atomically (foldr orElse retry $ waitSTM <$> xs)

cancel (Async _ tid) = killThread tid

getURL :: Request -> IO B8.ByteString
getURL s = getResponseBody <$> httpBS s

withAsync :: IO a -> (Async a -> IO c) -> IO c
withAsync io f = bracket (async io) cancel f

race :: IO a -> IO b -> IO (Either a b)
race a b = withAsync a $ \a -> withAsync b $ \b -> waitEither a b

concurrently :: IO a -> IO b -> IO (a, b)
concurrently a b = withAsync a $ \a -> withAsync b $ \b -> waitBoth a b

a1 = do
  xs <- traverse async (getURL <$> ["http://www.wikipedia.org/wiki/Shovel"])
  B8.length <$> waitAny xs >>= print

-- >>> a1