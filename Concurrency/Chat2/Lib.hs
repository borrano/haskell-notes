{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chat2.Lib where

import Chat2.Types
import Control.Concurrent.STM (retry)
import Control.Monad (forM_, forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Char (isPunctuation, isSpace)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Map qualified as M
import Data.Monoid (mappend)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy.Encoding (encodeUtf8)
import GHC.Conc (TVar, forkIO, newTVarIO)
import Network.WebSockets qualified as WS
import UnliftIO

newRoom name = Room name <$> newTChan

newState = atomically $ do
  room <- newRoom "default"
  users <- newTVar M.empty
  rooms <- newTVar (M.singleton "default" room)
  return $ State users rooms

startServer :: IO ()
startServer = do
  state <- newState
  WS.runServer "127.0.0.1" 9160 $ application state

application :: State -> WS.PendingConnection -> IO ()
application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) (checkName state conn)

checkName state conn = do
  WS.sendTextData conn ("Enter your user name" :: T.Text)
  let loop = do
        msg <- WS.receiveData conn
        mask $ \unmask -> do
          user <- addUserIfUniq state conn msg
          case user of
            Nothing -> unmask $ (WS.sendTextData conn ("Invalid User Name" :: T.Text) >> (loop))
            (Just user) -> (unmask (go user)) `finally` (atomically $ removeUser state user)
  loop
  where
    go user = void $ race (receive state user) (process state user)
    addUserIfUniq :: State -> WS.Connection -> T.Text -> IO (Maybe User)
    addUserIfUniq (State {..}) handle name = atomically $ do
      map <- readTVar users
      if M.member name map
        then return Nothing
        else do
          user <- User name handle <$> (newTVar M.empty) <*> newTChan <*> (newTVar S.empty)
          writeTVar users (M.insert name user map)
          return (Just user)

------------------------

leaveRoom :: State -> User -> RoomName -> STM ()
leaveRoom s@(State {..}) u@(User {..}) = undefined

getRooms :: State -> User -> STM [Room]
getRooms s@(State {..}) (User {..}) = undefined

getRoom s@(State {..}) roomName = M.lookup roomName <$> readTVar rooms

getUser s@(State {..}) userName = M.lookup userName <$> readTVar users

removeUser :: State -> User -> STM ()
removeUser s@(State {..}) u = do
  modifyTVar' users (M.delete (userName u))
  rooms' <- readTVar $ userRooms u
  traverse_ (leaveRoom s u) $ (M.keys rooms')

tell :: State -> User -> UserName -> T.Text -> STM ()
tell s from to msg = do
  user <- getUser s to
  case user of
    Nothing -> return ()
    (Just (User {userChan})) -> writeTChan userChan (STell (userName from) msg)

broadcast :: State -> RoomMessage -> STM ()
broadcast s msg = do
  room <- getRoom s (broom msg)
  case room of
    Nothing -> return ()
    (Just (Room {roombcastChan})) -> writeTChan roombcastChan msg

kick :: State -> User -> UserName -> RoomName -> STM ()
kick s from to roomName = do
  user <- getUser s to
  case user of
    Nothing -> return ()
    (Just (User {kickedRooms})) -> do
      modifyTVar kickedRooms (S.insert roomName)
      leave s from roomName

leave :: State -> User -> RoomName -> STM ()
leave s (User {..}) roomName = do
  modifyTVar userRooms (M.delete roomName)
  broadcast s (RoomLeave roomName userName)

join :: State -> User -> RoomName -> STM ()
join s (User {..}) roomName = do
  room <- getRoom s (roomName)
  case room of
    Nothing -> do
      -- create new room if it does not exists
      room@(Room {..}) <- newRoom roomName
      modifyTVar (rooms s) (M.insert roomName room)
      go roombcastChan
    (Just (Room {roombcastChan})) -> go roombcastChan
  where
    go chan = do
      newchan <- dupTChan chan
      modifyTVar userRooms (M.insert roomName newchan)
      broadcast s (RoomJoin roomName userName)
      
processClient :: State -> User -> Incoming -> STM [Outgoing]
processClient s u (CTell {..}) = tell s u cuser cmessage >> return []
processClient s u (CBroadcast {..}) = broadcast s (RoomMessage croom (userName u) cmessage) >> return []
processClient s u (CKick {..}) = kick s u cuser croom >> return []
processClient s u (CLeave {..}) = leave s u croom >> return []
processClient s u (CJoin {..}) = join s u croom >> return []
processClient s u _ = undefined

processOutput :: State -> User -> Message -> STM (Maybe [Outgoing])
processOutput s u STell {..} = return (Just [OTell suser smessage])
processOutput s u (SRoom roomMessage) = return (Just [ORoom roomMessage])
processOutput s u (SIncoming CQuit) = return Nothing
processOutput s u (SIncoming m) = (\x -> Just []) <$> processClient s u m

receive state u@(User {..}) = do
  line <- WS.receiveData userHandle
  atomically $ case decode (encodeUtf8 line) of
    Nothing -> return ()
    (Just msg) -> writeTChan userChan (SIncoming msg)
  receive state u

validAction kicked rooms m = case m of
  (SRoom m) -> go $ broom m
  (SIncoming (CBroadcast {..})) -> go croom
  (SIncoming (CKick {..})) -> go croom
  (SIncoming (CLeave {..})) -> go croom
  (SIncoming (CJoin {..})) -> S.notMember croom kicked && S.notMember croom rooms
  _ -> True
  where
    go r = S.notMember r kicked && S.member r rooms

process state u@(User {..}) = do
  kicked <- readTVarIO kickedRooms
  messages <- go kicked
  case messages of
    Nothing -> return ()
    (Just messages) -> traverse_ (WS.sendTextData userHandle . encode) messages >> process state u
  where
    go kicked = atomically $ do
      kicked' <- readTVar kickedRooms
      let dif = S.difference kicked' kicked
      if not $ S.null dif
        then return $ Just $ fmap (OKicked) $ S.toList dif
        else do
          rooms' <- readTVar userRooms
          output <- foldl orElse retry (fmap (fmap SRoom . readTChan) $ M.elems rooms') `orElse` (readTChan userChan)
          if validAction kicked' (M.keysSet rooms') output
            then processOutput state u output
            else return (Just [])