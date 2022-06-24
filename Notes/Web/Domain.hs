{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Domain where

import Control.Monad.Except
import Data.Aeson (ToJSON)
import Data.Char
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics
import Katip (KatipContext, Severity (InfoS), katipAddContext, logTM, ls, sl)
import Text.Regex.Base
import Text.Regex.PCRE

newtype Email = Email {getEmail :: T.Text} deriving (Show, Eq, Ord)

newtype Password = Password {getPassword :: T.Text} deriving (Show, Eq, Ord)

data InvalidEmail = InvalidEmail

data PasswordError
  = NoUppercase
  | NoNumber
  | NoLowercase
  deriving (Show, Eq, Ord)

mkEmail :: T.Text -> Either [InvalidEmail] Email
mkEmail = fmap Email . applyAll [\a -> if validate (T.encodeUtf8 a) then Nothing else Just InvalidEmail]
  where
    validate x = x =~ ("^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}$" :: String)

mkPassword :: T.Text -> Either [PasswordError] Password
mkPassword = fmap Password . applyAll [go isUpper NoUppercase, go isDigit NoNumber, go isLower NoNumber]
  where
    go f r xs = if T.any f xs then Nothing else Just r

applyAll fs a = case mapMaybe ($ a) fs of
  [] -> Right a
  xs -> Left xs

type VerificationCode = T.Text

data Auth = Auth
  { authEmail :: Email,
    authPassword :: Password
  }
  deriving (Show, Eq, Ord)

data RegistrationError = EmailTaken deriving (Show, Eq, Ord)

data EmailVerificationError = EmailVerificationError deriving (Show, Eq, Ord)

newtype UserId = UserId Int deriving (Show, Eq, Ord, Generic, Read)

instance ToJSON UserId

newtype SessionId = SessionId T.Text deriving (Show, Eq, Ord)

class Monad m => UserRepo m where
  addUser :: Auth -> m (Either RegistrationError (UserId, VerificationCode))
  markEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
  getUserIdByAuth :: Auth -> m (Maybe (UserId, Bool))
  getEmailByUserId :: UserId -> m (Maybe Email)
  getVerificationCodeByEmail :: Email -> m (Maybe VerificationCode)

class Monad m => EmailVerificationSender m where
  sendVerificationEmail :: Email -> VerificationCode -> m ()

class Monad m => SessionRepo m where
  newSession :: UserId -> m (SessionId)
  getUserBySessionId :: SessionId -> m (Maybe UserId)

withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext uId = katipAddContext (sl "userId" uId)

register :: (KatipContext m, UserRepo m, EmailVerificationSender m) => Auth -> m (Either RegistrationError ())
register reg@(Auth {..}) = runExceptT $ do
  (id, code) <- ExceptT $ addUser reg
  lift $ sendVerificationEmail authEmail code
  withUserIdContext id $
    $(logTM) InfoS $ ls (getEmail authEmail) <> " is registered successfully"

verifyEmail :: (KatipContext m, UserRepo m) => VerificationCode -> m (Either EmailVerificationError ())
verifyEmail = markEmailAsVerified

data LoginError = InvalidAuth | NotVerified

login :: (KatipContext m, UserRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = do
  x <- getUserIdByAuth auth
  case x of
    (Nothing) -> return $ Left InvalidAuth
    (Just (uid, False)) -> return $ Left NotVerified
    (Just (uid, True)) -> Right <$> newSession uid
