{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Db where

import Control.Exception
import Control.Lens (Magnify (magnify), Zoom (zoom), view)
import Control.Lens.TH (makeClassy, makeClassyPrisms)
import Control.Monad.Reader
import Data.ByteString (isInfixOf)
import Data.ByteString qualified as B
import Data.CaseInsensitive (CI (original))
import Data.Pool
import Data.Text qualified as T
import Data.Text.Encoding
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.Redis as Re
import System.Random
import Text.Read (readMaybe)
import UnliftIO (throwString)
import Web.Domain hiding (addUser, getEmailByUserId, getUserIdByAuth, getVerificationCodeByEmail, markEmailAsVerified)
import Web.Memory (User (email))

hello :: IO Int
hello = do
  conn <- connectPostgreSQL "postgresql://boran:boran@localhost/test"
  [Only i] <- query_ conn "select 2 + 2"
  return i

-- >>> hello
-- 4
--

-- >>> test5
-- Just "jfuipckxruwwjbrd"
--
data DbState = DbState
  { _pgConnection :: DbPool,
    _redisConnection :: RedisConn
  }

newtype DbPool = DbPool {_pool :: Pool PG.Connection}

newtype RedisConn = RedisConn {_conn :: Re.Connection}

makeClassy ''DbState

makeClassy ''DbPool
makeClassy ''RedisConn

instance HasDbPool DbState where
  dbPool = pgConnection

instance HasRedisConn DbState where
  redisConn = redisConnection

type State = Pool PG.Connection

type DBContext r m = (HasDbPool r, MonadIO m, MonadReader r m)

withConn :: (DBContext r m) => (PG.Connection -> IO b) -> m b
withConn action = do
  (DbPool pool) <- asks (view dbPool)
  liftIO . withResource pool $ \conn -> action conn

--
addUser :: DBContext r m => Auth -> m (Either RegistrationError (UserId, T.Text))
addUser (Auth {..}) = do
  verificationCode <- T.pack <$> (replicateM 16 $ randomRIO ('a', 'z'))
  result <- withConn $ \conn -> try $ query conn q (authEmail, authPassword, verificationCode)
  case result of
    Right [Only (uId :: UserId)] -> return $ Right (uId, verificationCode)
    Left err@SqlError {sqlState = "23505", sqlErrorMsg = msg}
      | "auths_email_key" `isInfixOf` msg -> return $ Left EmailTaken
    (Left err) -> throwString $ "Unhandled PG exception: " <> show err
    _ -> undefined
  where
    q =
      "insert into auths \
      \(email, pass, email_verification_code, is_email_verified) \
      \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"

--

getUserIdByAuth :: DBContext r m => Auth -> m (Maybe (UserId, Bool))
getUserIdByAuth (Auth {..}) = do
  result <- withConn $ \conn -> query conn q (authEmail, authPassword)
  case result of
    [((uid :: UserId), (b :: Bool))] -> return $ Just (uid, b)
    _ -> return $ Nothing
  where
    q =
      "select id, is_email_verified \
      \from auths \
      \where email = ? and pass = crypt(?, pass)"

--

getEmailByUserId :: DBContext r m => UserId -> m (Maybe Email)
getEmailByUserId uid = do
  result <- withConn $ \conn -> query conn q (Only uid)
  case result of
    [Only (email :: Email)] -> return $ Just email
    _ -> return $ Nothing
  where
    q =
      "select email \
      \from auths \
      \where id = ?"

--

getVerificationCodeByEmail :: DBContext r m => Email -> m (Maybe VerificationCode)
getVerificationCodeByEmail email = do
  result <- withConn $ \conn -> query conn q (Only email)
  case result of
    [Only (code :: VerificationCode)] -> return $ Just code
    _ -> return $ Nothing
  where
    q =
      "select email_verification_code \
      \from auths \
      \where email = ?;"

--

markEmailAsVerified :: DBContext r m => VerificationCode -> m (Either EmailVerificationError ())
markEmailAsVerified code = do
  result <- withConn $ \conn -> query conn q (Only code)
  case result of
    [(uid :: UserId, email :: Email)] -> return $ Right ()
    _ -> return $ Left $ EmailVerificationError
  where
    q =
      "UPDATE auths \
      \SET is_email_verified = 't' \
      \WHERE email_verification_code = ?\
      \returning id, email;"

--

instance ToField Email where
  toField (Email x) = toField x

instance ToField Password where
  toField (Password x) = toField x

instance ToField UserId where
  toField (UserId x) = toField x

instance FromField UserId where
  fromField field mdata = UserId <$> fromField field mdata

instance FromField Email where
  fromField field mdata = (Email . original) <$> fromField field mdata

-----------------------------------
type RedisContext r m = (HasRedisConn r, MonadIO m, MonadReader r m)

testRedis = do
  conn <- checkedConnect Re.defaultConnectInfo
  runRedis conn $ do
    set "hello" "hello"
    set "world" "world"
    hello <- get "hello"
    world <- get "world"
    liftIO $ print (hello, world)

withRedis action = do
  (RedisConn conn) <- asks (view redisConn)
  liftIO $ Re.runRedis conn $ action

sendVerificationEmail :: (MonadIO m, HasRedisConn s, MonadReader s m) => p1 -> p2 -> m ()
sendVerificationEmail _ _ = do
  res <- withRedis $ Re.publish "email" "asd"
  liftIO $ print res

newSession :: RedisContext r m => UserId -> m SessionId
newSession userId = do
  sid@(SessionId sId) <- (SessionId . T.pack) <$> (replicateM 16 $ randomRIO ('a', 'z'))
  result <- withRedis $ Re.set (encodeUtf8 sId) (encodeUtf8 . T.pack $ show $ userId)
  case result of
    Right Re.Ok -> return $ sid
    err -> throwString $ "Unexpected redis error: " <> show err

getUserBySessionId :: RedisContext r m => SessionId -> m (Maybe UserId)
getUserBySessionId (SessionId id) = do
  result <- withRedis $ Re.get (encodeUtf8 id)
  case result of
    Right (Just str) -> (liftIO $ print (show str)) >> (return $ (readMaybe . T.unpack . decodeUtf8) str)
    err -> throwString $ "Unexpected redis error: " <> show err

-- >>> testRedis
-- (Right (Just "hello"),Right (Just "world"))
--
