{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Web.Lib where

import Control.Concurrent (threadDelay)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Pool
import Data.Text qualified as T
import Database.PostgreSQL.Simple
import Database.Redis as Re (Message (msgChannel), checkedConnect, defaultConnectInfo, disconnect, pubSub, runRedis, subscribe)
import Katip
import Network.Wai.Handler.Warp (run)
import Servant
import System.IO
import System.Random
import UnliftIO hiding (Handler)
import Web.Db (DbPool (DbPool))
import Web.Db qualified as Db
import Web.Domain
import Web.Memory qualified as Mem
import Web.Web (API, customFormatters, server)

newtype MemoryApp a = MemoryApp
  { runApp :: KatipContextT (ReaderT (Mem.InMemoryState) IO) a
  }
  deriving (Applicative, Functor, Monad, MonadReader (Mem.InMemoryState), MonadIO, MonadFail, KatipContext, Katip)

instance UserRepo MemoryApp where
  addUser = Mem.addUser
  markEmailAsVerified = Mem.markEmailAsVerified
  getUserIdByAuth = Mem.getUserIdByAuth
  getEmailByUserId = Mem.getEmailByUserId
  getVerificationCodeByEmail = Mem.getVerificationCodeByEmail

instance EmailVerificationSender MemoryApp where
  sendVerificationEmail = Mem.sendVerificationEmail

instance SessionRepo MemoryApp where
  newSession = Mem.newSession
  getUserBySessionId = Mem.getUserBySessionId

runMemory :: LogEnv -> Mem.InMemoryState -> MemoryApp a -> IO a
runMemory le state = flip runReaderT state . runKatipContextT le () mempty . runApp

newtype DbApp a = DbApp
  { runDbApp :: KatipContextT (ReaderT (Db.DbState) (ExceptT ServerError IO)) (a)
  }
  deriving (Applicative, Functor, Monad, MonadReader (Db.DbState), MonadIO, MonadFail, KatipContext, Katip, MonadError ServerError)

instance UserRepo (DbApp) where
  addUser = Db.addUser
  markEmailAsVerified = Db.markEmailAsVerified
  getUserIdByAuth = Db.getUserIdByAuth
  getEmailByUserId = Db.getEmailByUserId
  getVerificationCodeByEmail = Db.getVerificationCodeByEmail

instance EmailVerificationSender (DbApp) where
  sendVerificationEmail = Db.sendVerificationEmail

instance SessionRepo (DbApp) where
  newSession = Db.newSession
  getUserBySessionId = Db.getUserBySessionId

runDb le state = flip runReaderT state . runKatipContextT le () mempty . runDbApp

testAction = do
  mail <- T.pack <$> (replicateM 5 $ randomRIO ('a', 'z'))

  let email = either undefined id $ mkEmail $ mail <> "@borrano.com"
      passw = either undefined id $ mkPassword "1234ABCDefgh"
      auth = Auth email passw
  $(logTM) InfoS "Started"
  register auth

  (Just code) <- getVerificationCodeByEmail email
  verifyEmail code
  Right session <- login auth
  liftIO $ print session
  Just uId <- getUserBySessionId session
  Just registeredEmail <- getEmailByUserId uId

  liftIO $ print (session, uId, registeredEmail) >> threadDelay 1000000

withKatip app =
  bracket createLogEnv closeScribes app
  where
    createLogEnv = do
      logEnv <- initLogEnv "HAuth" "prod"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

withPool action =
  bracket initPool cleanPool action
  where
    initPool = createPool openConn closeConn 10 (10) 10
    cleanPool = destroyAllResources
    openConn = connectPostgreSQL ("postgresql://boran:boran@localhost/test")
    closeConn = close

withRedis action =
  bracket (checkedConnect Re.defaultConnectInfo) disconnect action

testMemory action = do
  s <- atomically $ Mem.newInMemoryState
  withKatip $ \le -> runMemory le s action

testDb action = do
  withKatip $ \le -> withPool $ \pool ->
    withRedis $ \r -> (action (le, (Db.DbState (Db.DbPool pool) (Db.RedisConn r))))
  where
    sub = pubSub (Re.subscribe ["email"]) $ \msg -> do
      putStrLn $ "Message from " ++ show (Re.msgChannel msg)
      return mempty

webApp = testDb $
  \(a, b) ->
    run 8081 $
      serveWithContext (Proxy @API) (customFormatters Servant.:. EmptyContext) $
        (hoistServer (Proxy @API) (\(x :: DbApp a) -> Handler $ runDb a b x) server)

-- >>> testDb testAction
-- [2022-04-12 15:58:46][HAuth][Info][boran-MS-7C84][PID 107413][ThreadId 13593][main:Web.Lib /media/boran/hori/github/haskell/Notes/src/Web/Lib.hs:69:4] Started
-- SessionId "hflmniourqmwcpby"
-- "\"UserId 2\""
-- (SessionId "hflmniourqmwcpby",UserId 2,Email {getEmail = "borrano@borrano.com"})
--

testEmail = either undefined id $ mkEmail "borranao@borrano.com"

testAuth =
  let passw = either undefined id $ mkPassword "1234ABCDefgh"
   in Auth testEmail passw

-- test1 = testDb $ \r -> runExceptT $ r (addUser (testAuth))
--
-- test2 = testDb $ \r -> runExceptT $ r (getUserIdByAuth (testAuth))
--
-- test3 = testDb $ \r -> runExceptT $ r (getEmailByUserId (UserId 6))
--
-- test4 = testDb $ \r -> runExceptT $ r $ markEmailAsVerified ("jfuipckxruwwjbrd" :: T.Text)
--
-- test5 = testDb $ \r -> runExceptT $ r $ getVerificationCodeByEmail testEmail

-- >>> test5
-- Just "jfuipckxruwwjbrd"
--
