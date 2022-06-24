{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Web.Memory where

import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (find)
import Data.Map qualified as M
import Data.Text qualified as T
import System.Random
import Web.Domain

data User = User
  { userId :: UserId,
    email :: Email,
    password :: Password,
    verificationCode :: VerificationCode,
    verified :: Bool
  }

data InMemoryState = InMemoryState
  { users :: TVar [User],
    counter :: TVar Int,
    sessions :: TVar (M.Map SessionId UserId)
  }

newInMemoryState = InMemoryState <$> newTVar [] <*> newTVar 1 <*> newTVar (M.empty)

get :: Foldable t => (a -> Bool) -> TVar (t a) -> STM (Maybe a)
get p users = do
  users' <- readTVar users
  return $ find p users'

getIncreaseCounter (InMemoryState {..}) = do
  c <- lift $ readTVar counter
  lift $ modifyTVar' counter (+ 1)
  return c

addUser (Auth {..}) = do
  state@(InMemoryState {..}) <- ask
  randomCode <- T.pack <$> (replicateM 16 $ randomRIO ('a', 'z'))
  (liftIO . atomically . runExceptT) $ do
    x <- lift $ get (\u -> email u == authEmail) users
    case x of
      Nothing -> do
        c <- UserId <$> getIncreaseCounter state
        lift $ modifyTVar (users) ((User (c) authEmail authPassword (randomCode) False) :)
        return (c, randomCode)
      _ -> throwError EmailTaken

getUserIdByAuth (Auth {..}) = do
  (InMemoryState {..}) <- ask
  res <- liftIO $ atomically $ get (\u -> (email u == authEmail) && password u == authPassword) users
  case res of
    Nothing -> return Nothing
    (Just (User {..})) -> return $ Just (userId, verified)

getEmailByUserId uid = do
  (InMemoryState {..}) <- ask
  res <- liftIO $ atomically $ get (\u -> userId u == uid) users
  return (email <$> res)

getVerificationCodeByEmail e = do
  (InMemoryState {..}) <- ask
  res <- liftIO $ atomically $ get (\u -> email u == e) users
  return (verificationCode <$> res)

markEmailAsVerified code = do
  InMemoryState {..} <- ask
  liftIO $
    atomically $
      runExceptT $ do
        us <- lift $ readTVar users
        let (us', b) = foldl (\(xs, b) u -> if verificationCode u == code then (u {verified = True} : xs, True) else (u : xs, b)) ([], False) us
        if not b then throwError EmailVerificationError else lift $ writeTVar users us'

sendVerificationEmail email _ = return ()

newSession userid = do
  InMemoryState {..} <- ask
  sessionId <- (SessionId . T.pack) <$> (replicateM 16 $ randomRIO ('a', 'z'))
  liftIO $ atomically $ modifyTVar sessions (M.insert sessionId userid)
  return sessionId

getUserBySessionId sessionId = do
  InMemoryState {..} <- ask
  liftIO $ atomically $ M.lookup sessionId <$> readTVar sessions

-- >>> k
-- (Error while loading modules for evaluation)
-- [1 of 1] Compiling Web.Memory       ( /media/boran/hori/github/haskell/Notes/src/Web/Memory.hs, interpreted )
-- <BLANKLINE>
-- /media/boran/hori/github/haskell/Notes/src/Web/Memory.hs:6:1-29: error:
--     Could not load module ‘Control.Concurrent.STM’
--     It is a member of the hidden package ‘stm-2.5.0.0’.
--     You can run ‘:set -package stm’ to expose it.
--     (Note: this unloads all the modules in the current scope.)
--     Use -v (or `:set -v` in ghci) to see a list of the files searched for.
-- <BLANKLINE>
-- /media/boran/hori/github/haskell/Notes/src/Web/Memory.hs:7:1-27: error:
--     Could not load module ‘Control.Monad.Except’
--     It is a member of the hidden package ‘mtl-2.2.2’.
--     You can run ‘:set -package mtl’ to expose it.
--     (Note: this unloads all the modules in the current scope.)
--     Use -v (or `:set -v` in ghci) to see a list of the files searched for.
-- <BLANKLINE>
-- /media/boran/hori/github/haskell/Notes/src/Web/Memory.hs:8:1-27: error:
--     Could not load module ‘Control.Monad.Reader’
--     It is a member of the hidden package ‘mtl-2.2.2’.
--     You can run ‘:set -package mtl’ to expose it.
--     (Note: this unloads all the modules in the current scope.)
--     Use -v (or `:set -v` in ghci) to see a list of the files searched for.
-- <BLANKLINE>
-- /media/boran/hori/github/haskell/Notes/src/Web/Memory.hs:10:1-30: error:
--     Could not load module ‘Data.Map’
--     It is a member of the hidden package ‘containers-0.6.5.1’.
--     You can run ‘:set -package containers’ to expose it.
--     (Note: this unloads all the modules in the current scope.)
--     Use -v (or `:set -v` in ghci) to see a list of the files searched for.
-- <BLANKLINE>
-- /media/boran/hori/github/haskell/Notes/src/Web/Memory.hs:11:1-31: error:
--     Could not load module ‘Data.Text’
--     It is a member of the hidden package ‘text-1.2.5.0’.
--     You can run ‘:set -package text’ to expose it.
--     (Note: this unloads all the modules in the current scope.)
--     Use -v (or `:set -v` in ghci) to see a list of the files searched for.
-- <BLANKLINE>
-- /media/boran/hori/github/haskell/Notes/src/Web/Memory.hs:12:1-20: error:
--     Could not load module ‘System.Random’
--     It is a member of the hidden package ‘random-1.2.1’.
--     You can run ‘:set -package random’ to expose it.
--     (Note: this unloads all the modules in the current scope.)
--     Use -v (or `:set -v` in ghci) to see a list of the files searched for.
-- <BLANKLINE>
-- /media/boran/hori/github/haskell/Notes/src/Web/Memory.hs:13:1-17: error:
--     Could not load module ‘Web.Domain’
--     It is a member of the hidden package ‘Notes-0.1.0.0’.
--     You can run ‘:set -package Notes’ to expose it.
--     (Note: this unloads all the modules in the current scope.)
--     Use -v (or `:set -v` in ghci) to see a list of the files searched for.
-- Failed, no modules loaded.
--
