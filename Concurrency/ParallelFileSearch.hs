{-# LANGUAGE ScopedTypeVariables #-}

module ParallelFileSearch where

import Control.Concurrent hiding (signalQSem)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.List
import Debug.Trace
import Semaphores (NBSem (new, signalQSem, tryWaitQSem), NBSemIORef)
import System.Directory

getDirectoryContents' d = (getDirectoryContents d `catch` (\(e :: SomeException) -> return []))

sequential :: String -> FilePath -> IO (Maybe FilePath)
sequential filename directory = do
  xs <- sort . filter (`notElem` [".", ".."]) <$> getDirectoryContents' directory

  if filename `elem` xs
    then return $ Just (directory ++ "/" ++ filename)
    else go (fmap ((directory ++ "/") ++) xs)
  where
    go :: [FilePath] -> IO (Maybe FilePath)
    go [] = return Nothing
    go (d : ds) = do
      b <- doesDirectoryExist d
      if b then sequential filename d >>= maybe (go ds) (return . Just) else go ds

parallel :: String -> FilePath -> IO (Maybe FilePath)
parallel filename directory = do
  xs <- sort . filter (`notElem` [".", ".."]) <$> getDirectoryContents' directory
  if filename `elem` xs
    then return $ Just (directory ++ "/" ++ filename)
    else foldr f init (fmap ((directory ++ "/") ++) xs) []
  where
    f d g asyncs = do
      b <- doesDirectoryExist d
      if b then withAsync (parallel filename d) (\a -> g $ a : asyncs) else g asyncs
    init xs = go (reverse xs)
      where
        go [] = return Nothing
        go (x : xs) = wait x >>= maybe (go xs) (return . Just)

-- >>> :set +s
-- >>> sequential "AsyncMVar.hs" "/media/boran/hori"
-- (0.00 secs, 794,832 bytes)
-- Just "/media/boran/hori/github/haskellconcurrencybook/haskellconcurrencybook/src/AsyncMVar.hs"
-- (1.00 secs, 2,003,926,024 bytes)
--

-- >>> parallel "AsyncMVar.hs" "/media/boran/hori"
-- (0.00 secs, 795,680 bytes)
-- Just "/media/boran/hori/github/haskellconcurrencybook/haskellconcurrencybook/src/AsyncMVar.hs"
-- (2.45 secs, 954,272 bytes)
--

parallelSem :: NBSem a => a -> String -> FilePath -> IO (Maybe FilePath)
parallelSem sem filename directory = do
  xs <- sort . filter (`notElem` [".", ".."]) <$> getDirectoryContents' directory
  if filename `elem` xs
    then return $ Just (directory ++ "/" ++ filename)
    else foldr f init (fmap ((directory ++ "/") ++) xs) []
  where
    f d g asyncs = do
      b <- doesDirectoryExist d
      if not b
        then g asyncs
        else do
          b <- tryWaitQSem sem
          if b
            then withAsync (parallel filename d `finally` signalQSem sem) (\a -> g $ a : asyncs)
            else parallelSem sem filename d >>= maybe (g asyncs) (return . Just)

    init xs = go (reverse xs)
      where
        go [] = return Nothing
        go (x : xs) = wait x >>= maybe (go xs) (return . Just)

test :: IO ()
test = do
  n <- getNumCapabilities
  print n
  (sem :: NBSemIORef) <- new (n * 4)
  parallelSem sem "AsyncMVar1.hs" "/media/boran/hori" >>= print
