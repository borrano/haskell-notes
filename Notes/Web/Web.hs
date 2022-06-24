{-# LANGUAGE DataKinds #-}

module Web.Web where

import Control.Monad.Except
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), Value (Object), object, withObject, (.:))
import Data.String.Conversions
import GHC.Generics
import Katip
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.API.ContentTypes
import Web.Domain (Auth (..), EmailVerificationSender, SessionId, UserRepo)

type App m = (KatipContext m, UserRepo m, EmailVerificationSender m, MonadError ServerError m)

type API =
  "api" :> "auth"
    :> ( "register" :> ReqBody '[JSON] X :> Get '[JSON] String
           :<|> "register2" :> Get '[JSON] NoContent
       )

data X = X {a :: Int} deriving (Generic)

instance FromJSON X

-- instance FromJSON Auth where
--  parseJSON (Object v) = do
--    etext <- v .: "email"
--    epass <- v .: "password"
--
--    Auth <$> (Email <$> (v .: "email")) <*> (Password <$> (v .: "age"))
--  parseJSON _ = mempty

-- Auth <$> undefined <*>
--
bodyErrorFormatter :: ErrorFormatter
bodyErrorFormatter tr req err =
  let value = object ["combinator" .= show tr, "error" .= err]
      accH = getAcceptHeader req
   in case handleAcceptH (Proxy :: Proxy '[JSON]) accH value of
        -- If client can't handle JSON, we just return the body the old way
        Nothing -> err400 {errBody = cs err}
        -- Otherwise, we return the JSON formatted body and set the "Content-Type" header.
        Just (ctypeH, body) ->
          err400
            { errBody = body,
              errHeaders = [("Content-Type", cs ctypeH)]
            }

customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters {bodyParserErrorFormatter = bodyErrorFormatter}

-- curl -X GET -d '{"a": "asd"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/api/auth/register
server :: forall m. (App m) => ServerT API m
server = register :<|> register2
  where
    register :: X -> m String
    register x = return "asd"
    register2 :: m NoContent
    register2 = return NoContent
