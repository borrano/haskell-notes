{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Chat.Chat1 where

import Chat.Shared (runTCPServer)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.Map as M
import GHC.IO.Handle
import Network.Socket (SocketOption (Broadcast))
import System.IO (hPrint)

startChat :: IO ()
startChat = do
  state <- newState
  runTCPServer Nothing "4444" state loop
  where
    newState :: IO State
    newState = State <$> newTVarIO M.empty

data User = User
  { name :: String,
    h :: Handle,
    kicked :: TVar Bool,
    chan :: TChan Message
  }

newtype State = State (TVar (M.Map String User))

addUserIfUniq :: State -> Handle -> String -> STM (Maybe User)
addUserIfUniq (State s) h name = do
  map <- readTVar s
  if M.member name map
    then return Nothing
    else do
      user <- User name h <$> newTVar False <*> newTChan
      writeTVar s (M.insert name user map)
      return (Just user)

removeUser :: State -> User -> STM ()
removeUser s'@(State s) (User {..}) = do
  modifyTVar' s (M.delete name)
  broadcast s' (Right $ SNotice $ name ++ "left the chat")

data ClientCommand = CTell String String | CBroadcast String | CKick String

data ServerCommand = STell String String | SBroadCast String String | SNotice String

type Message = Either ClientCommand ServerCommand

tell :: State -> String -> String -> String -> STM ()
tell (State s) from to msg = do
  map <- readTVar s
  case M.lookup to map of
    Nothing -> return ()
    (Just (User {chan, ..})) -> writeTChan chan (Right $ STell from msg)

broadcast :: State -> Message -> STM ()
broadcast (State s) msg = do
  m <- readTVar s
  traverse_ (\(User {chan, ..}) -> writeTChan chan msg) m

kick :: State -> String -> STM ()
kick (State s) who = do
  map <- readTVar s
  case M.lookup who map of
    Nothing -> return ()
    (Just (User {kicked, ..})) -> writeTVar kicked True

loop :: State -> Handle -> IO ()
loop s h = do
  hPrint h "user name:"
  name <- hGetLine h
  case name of
    [] -> loop s h
    name -> mask $ \unmask -> do
      r <- atomically $ addUserIfUniq s h name
      case r of
        Nothing -> unmask $ loop s h
        (Just user) -> void $ unmask (race (receive user) (process user)) `finally` atomically (removeUser s user)
  where
    receive u@(User {..}) = do
      line <- hGetLine h
      cont <- atomically $ case words line of
        ("tell" : who : what : _) -> writeTChan chan (Left $ CTell who what) >> return True
        ("kick" : who : _) -> writeTChan chan (Left $ CKick who) >> return True
        ("broadcast" : what : _) -> writeTChan chan (Left $ CBroadcast what) >> return True
        ("quit" : _) -> return False
        _ -> return True
      when cont $ receive u
    process u@(User {..}) = do
      (b, io) <- atomically $ do
        b <- readTVar kicked
        if b
          then return (False, output "you are kicked")
          else do
            msg <- readTChan chan
            io <- case msg of
              (Left (CTell to message)) -> tell s name to message $> return ()
              (Left (CBroadcast message)) -> broadcast s (Right $ SBroadCast name message) $> return ()
              (Left (CKick who)) -> kick s who $> return ()
              (Right (STell from msg)) -> return $ output $ "message: " ++ from ++ ":" ++ msg
              (Right (SBroadCast from msg)) -> return $ output $ "broadcast: " ++ from ++ ":" ++ msg
              (Right (SNotice msg)) -> return $ output $ "notice: " ++ msg where
            return (True, io)
      io
      when b $ process u
    output = hPrint h
