{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Auth where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Data
import qualified Data.Map as M
import Data.Text
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (Day, UTCTime, fromGregorian)
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.Client
import Servant.Types.SourceT hiding (readFile)

type Username = T.Text

type Password = T.Text

type Website = T.Text

data User = User
  { user :: Username,
    pass :: Password,
    site :: Website
  }
  deriving (Eq, Show)

-- could be a postgres connection, a file, anything.
type UserDB = M.Map Username User

-- create a "database" from a list of users
createUserDB :: [User] -> UserDB
createUserDB users = M.fromList [(user u, u) | u <- users]

-- our test database
userDB :: UserDB
userDB =
  createUserDB
    [ User "john" "shhhh" "john.com",
      User "foo" "bar" "foobar.net"
    ]

-- a 'GET /mysite' endpoint, protected by basic authentication
type API = BasicAuth "People's websites" User :> "mysite" :> Get '[JSON] Website

{- if there were more endpoints to be protected, one could write:
type API = BasicAuth "People's websites" User :>
    ( "foo" :> Get '[JSON] Foo
 :<|> "bar" :> Get '[JSON] Bar
    )
-}

api :: Proxy API
api = Proxy

server :: Server API
server usr = return (site usr)

-- provided we are given a user database, we can supply
-- a function that checks the basic auth credentials
-- against our database.
checkBasicAuth :: UserDB -> BasicAuthCheck User
checkBasicAuth db = BasicAuthCheck $ \basicAuthData ->
  let username = decodeUtf8 (basicAuthUsername basicAuthData)
      password = decodeUtf8 (basicAuthPassword basicAuthData)
   in case M.lookup username db of
        Nothing -> return NoSuchUser
        Just u ->
          if pass u == password
            then return (Authorized u)
            else return BadPassword

runApp :: UserDB -> IO ()
runApp db = run 8080 (serveWithContext api ctx server)
  where
    ctx = checkBasicAuth db :. EmptyContext

-----------------------------------------------------------
-- client
getSite :: BasicAuthData -> ClientM Website
getSite = client api

main :: IO ()
main = do
  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runApp userDB) killThread $ \_ ->
    runClientM (getSite u) (mkClientEnv mgr (BaseUrl Http "localhost" 8080 ""))
      >>= print
  where
    u = BasicAuthData "foo" "bar"