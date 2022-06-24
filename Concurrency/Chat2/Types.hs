{-# LANGUAGE DeriveAnyClass #-}

module Chat2.Types where

import Control.Concurrent.STM
import Data.Aeson
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text
import GHC.Generics
import Network.WebSockets qualified as WS

type UserName = Text

type RoomName = Text

data Incoming
  = CTell {cuser :: UserName, cmessage :: Text}
  | CBroadcast {croom :: RoomName, cmessage :: Text}
  | CKick {croom :: RoomName, cuser :: UserName}
  | CLeave {croom :: RoomName}
  | CJoin {croom :: RoomName} -- if room does not exists it is created,
  | CQuit
  deriving (Generic, Show, ToJSON, FromJSON)

data RoomMessage
  = RoomMessage {broom :: RoomName, buser :: UserName, bmessage :: Text}
  | RoomJoin {broom :: RoomName, buser :: UserName}
  | RoomLeave {broom :: RoomName, buser :: UserName}
  deriving (Generic, Show, ToJSON, FromJSON)

data Outgoing
  = OKicked {oroom :: RoomName}
  | ORoom RoomMessage
  | OTell {ouser :: UserName, omessage :: Text}
  deriving (Generic, Show, ToJSON, FromJSON)

data Message
  = STell {suser :: UserName, smessage :: Text}
  | SRoom RoomMessage
  | SIncoming Incoming
  deriving (Generic, Show, ToJSON, FromJSON)

data User = User
  { userName :: UserName,
    userHandle :: WS.Connection,
    userRooms :: TVar (M.Map RoomName (TChan RoomMessage)),
    userChan :: TChan Message,
    kickedRooms :: TVar (S.Set RoomName)
  }

data Room = Room
  { roomName :: RoomName,
    roombcastChan :: TChan RoomMessage
  }

data State = State
  { users :: TVar (M.Map UserName User),
    rooms :: TVar (M.Map RoomName Room)
  }