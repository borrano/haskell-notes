{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module AsyncMVar where

import Control.Concurrent
import Control.Exception (AsyncException (ThreadKilled), SomeException, mask, throwIO, try)
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B8
import Data.Foldable
import Network.HTTP.Simple
import System.CPUTime

getURL :: Request -> IO B8.ByteString
getURL s = getResponseBody <$> httpBS s

async1 = do
  mvar1 <- newEmptyMVar
  forkIO $ getURL "http://www.wikipedia.org/wiki/Shovel" >>= putMVar mvar1
  mvar2 <- newEmptyMVar
  forkIO $ getURL "http://www.wikipedia.org/wiki/Shovel" >>= putMVar mvar2
  r1 <- takeMVar mvar1
  r2 <- takeMVar mvar2
  print (B8.length r1, B8.length r2) -- 7

-- >>> async1
-- (106705,106705)
--

data Async a = Async (MVar (Either SomeException a)) ThreadId

forkFinally' io after = mask $ \unmask -> forkIO (try (unmask io) >>= after)

async io = do
  mvar1 <- newEmptyMVar
  tid <- forkFinally' io $ putMVar mvar1
  return $ Async mvar1 tid

waitCatch (Async a _) = readMVar a

wait (Async a _) = readMVar a >>= either throwIO return

cancel (Async _ i) = throwTo i ThreadKilled

--waitEither :: Async a -> Async b -> IO (Async (Either a b))
--waitEither (Async a) (Async b) = do
--  mvar <- newEmptyMVar
--  forkIO $ undefined

async2 = do
  a1 <- async $ B8.length <$> getURL "http://www.wikipedia.org/wiki/Shovel"
  a2 <- async $ B8.length <$> getURL "http://www.wikipedia.org/wiki/Vovel"
  (,) <$> wait a1 <*> wait a2

-- >>> async2

timeIt :: MonadIO m => m a -> m (Double, a)
timeIt ioa = do
  t1 <- liftIO getCPUTime
  a <- ioa
  t2 <- liftIO getCPUTime
  let t :: Double
      t = fromIntegral (t2 - t1) * 1e-12
  return (t, a)

sites =
  [ "http://www.google.com",
    "http://www.bing.com",
    "http://www.yahoo.com",
    "http://www.wikipedia.com/wiki/Spade",
    "http://www.wikipedia.com/wiki/Shovel"
  ]

timed = timeIt . fmap B8.length . getURL >=> print

compare1 = do
  xs <- traverse (async . timed) sites
  traverse_ wait xs

-- >>> compare1
-- (9.263002e-3,82000)
-- (2.0600705e-2,49771)
-- (4.6165454e-2,794312)
-- (5.3120216e-2,75053)
-- (5.4647107e-2,106705)
-- [(),(),(),(),()]
--