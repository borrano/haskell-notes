{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module STM where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

makeDisplay = foldM go M.empty
  where
    go map (id, windows) = do
      w' <- newTVar (S.fromList windows)
      return $ M.insert id w' map

display focus state = atomically readFocus >>= go
  where
    readFocus = readTVar focus >>= readTVar . (state M.!)
    go windows = do
      print windows
      wins' <- atomically $ do
        windows' <- readFocus
        if windows' == windows then retry else return windows'
      go wins'

changeFocus tvar a = atomically $ writeTVar tvar a

moveWindow state from to window = do
  modifyTVar (state M.! from) (S.delete window)
  modifyTVar (state M.! to) (S.insert window)

stm1 = do
  focus <- newTVarIO 0
  state <- atomically $ makeDisplay [(0, ["a", "b"]), (1, ["c", "d"])]
  forkIO $ display focus state
  threadDelay 1000000
  changeFocus focus 1
  threadDelay 1000000
  atomically $ moveWindow state 0 1 "a"
  threadDelay 1000000
  atomically $ moveWindow state 0 1 "b"

  return ()

-----------------------------------------------------------------------------------------
-- TMVar

newtype TMVar' a = TMVar' (TVar (Maybe a))

newEmptyTMVar' = TMVar' <$> newTVarIO Nothing

takeTMVar' (TMVar' x) = readTVar x >>= maybe retry (\a -> writeTVar x Nothing >> return a)

putTMVar' (TMVar' x) a = readTVar x >>= maybe (writeTVar x (Just a)) (const retry)

----------------------------------------------------------------------------------------------
-- Channels

data Chan1 a = Chan1 {start :: TVar (TVar (TList a)), end :: TVar (TVar (TList a))}

data TList a = Cons a (TVar (TList a)) | E

new1 = do
  hole <- newTVar E
  Chan1 <$> newTVar hole <*> newTVar hole

push1 (Chan1 start end) elem = do
  newEnd <- newTVar E
  writeTVar newEnd (Cons elem newEnd)
  writeTVar end newEnd

pop1 (Chan1 start end) = do
  node <- readTVar start
  list <- readTVar node
  case list of
    E -> retry
    (Cons a rest) -> writeTVar start rest >> return a

unpop1 (Chan1 start end) elem = do
  node <- readTVar start
  e <- newTVar (Cons elem node)
  writeTVar start e

isEmpty1 (Chan1 start end) = readTVar start >>= fmap f . readTVar
  where
    f E = True
    f _ = False

dup1 (Chan1 start end) = do
  node <- readTVar end
  newStart <- newTVar node
  return $ Chan1 newStart end

------------------
--Exceptions

-- all stm effects are discarded
e1 = do
  tvar <- newTVarIO 1
  handle (\(e :: SomeException) -> return ()) $ atomically $ writeTVar tvar 2 >> throwSTM (ErrorCall "asd")
  readTVarIO tvar

-- >>> e1
-- 1
--


