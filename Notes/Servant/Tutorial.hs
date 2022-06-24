{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Tutorial where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Data
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, UTCTime, fromGregorian)
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.Types.SourceT hiding (readFile)

--type API =
--
--    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
--    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
-- curl http://localhost:8081/users
-- curl http://localhost:8081/user/1
-- curl http://localhost:8081/queryparam?userId=1
-- curl -G http://localhost:8081/queryparams -d "userIds=0&userIds=1"  -- http://localhost:8081/queryparams?userIds=0&userIds=1
-- curl -X POST -d '{"name":"x", "age":12, "email":"x@x", "registration_date":"1683-03-01"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/adduser
-- curl -X POST -d '{"name":"x", "age":12, "email":"x@x", "registration_date":"1683-03-01"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/adduser2
-- curl http://localhost:8081/myfile.txt
-- curl -X HEAD -i    http://localhost:8081/withHeader/1 -- X-An-Int: 12
-- curl http://localhost:8081/static/Main.hs
-- curl --header "Authorization: 123"   http://localhost:8081/nestedUser/1
-- curl http://localhost:8081/checkReader
-- curl http://localhost:8081/stream

data MyJSON

instance Accept MyJSON where
  contentType _ = "application" // "json"

instance ToJSON a => MimeRender MyJSON a where
  mimeRender _ = encode

instance (FromJSON a) => MimeUnrender MyJSON a where
  mimeUnrender _ = eitherDecode

type UserAPI =
  "users" :> Get '[JSON] [User]
    :<|> "user" :> Capture "userId" Int :> Get '[JSON] User
    :<|> "queryparam" :> QueryParam "userId" Int :> Get '[JSON] User
    :<|> "queryparams" :> QueryParams "userIds" Int :> Get '[JSON] [User]
    :<|> "adduser" :> ReqBody '[JSON] User :> Post '[JSON] User
    :<|> "adduser2" :> ReqBody '[MyJSON] User :> Post '[MyJSON] User
    :<|> "myfile.txt" :> Get '[JSON] FileContent
    :<|> "withHeader" :> Capture "userId" Int :> Get '[JSON] (Headers '[Header "X-An-Int" Int] User)
    :<|> "static" :> Raw
    :<|> NestedAPI
    :<|> "checkReader" :> Get '[JSON] String
    :<|> StreamAPI

type NestedAPI = "nestedUser" :> Header "Authorization" Token :> Capture "userId" Int :> (Get '[JSON] String :<|> DeleteNoContent)

type StreamAPI = "stream" :> StreamGet NewlineFraming JSON (SourceIO User)

newtype Token = Token ByteString deriving (Generic, Show)

instance FromHttpApiData Token where
  parseHeader = Right . Token
  parseQueryParam = Right . Token . encodeUtf8

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype FileContent = FileContent {content :: String} deriving (Generic, FromJSON, ToJSON)

userList = [User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1), User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)]

type MyMonad = ReaderT String Handler

server :: ServerT UserAPI MyMonad
server =
  users :<|> user :<|> queryparam :<|> queryparams :<|> addUser :<|> addUser2
    :<|> file
    :<|> withHeader
    :<|> static
    :<|> nested
    :<|> checkReader
    :<|> stream
  where
    users :: MyMonad [User]
    users = return userList
    user :: Int -> MyMonad User
    user i = return $ userList !! i
    queryparam :: Maybe Int -> MyMonad User
    queryparam Nothing = throwError $ err503 {errBody = "Sorry dear user."}
    queryparam (Just i) = return $ userList !! i
    queryparams :: [Int] -> MyMonad [User]
    queryparams xs = liftIO (print xs) >> return (fmap (userList !!) xs)
    addUser :: User -> MyMonad User
    addUser = return
    addUser2 :: User -> MyMonad User
    addUser2 = return
    file :: MyMonad FileContent
    file = FileContent <$> liftIO (readFile "./LICENSE")
    withHeader :: Int -> MyMonad (Headers '[Header "X-An-Int" Int] User)
    withHeader id = return $ addHeader 12 $ userList !! id
    static :: ServerT Raw MyMonad
    static = serveDirectoryWebApp "app"
    nested :: Maybe Token -> Int -> MyMonad String :<|> MyMonad NoContent
    nested Nothing i = throwError err404 {errBody = "errorrrr!."} :<|> throwError err404 {errBody = "errorrrr!."}
    nested (Just t) i = return (show t) :<|> undefined
    checkReader :: MyMonad String
    checkReader = ask
    stream :: MyMonad (SourceT IO User)
    stream = return $ source userList
    basicAuth = undefined

app1 :: Application
app1 = serve (Proxy @UserAPI) (hoistServer (Proxy @UserAPI) (`runReaderT` "boran") server)

servantEx1 :: IO ()
servantEx1 = run 8081 app1