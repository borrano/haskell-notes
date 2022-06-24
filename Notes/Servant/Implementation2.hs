{-# LANGUAGE DataKinds #-}

module Servant.Implementation2 where

import Control.Monad.Except (ExceptT)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
  ( Get,
    Handler,
    JSON,
    Proxy (..),
    ServerError,
    ServerT,
    serve,
    (:<|>) ((:<|>)),
    (:>),
  )
import GHC.TypeLits

-- | A representation of our REST API at the type level.
--
-- This defines two routes:
--   * /dogs -- Responds to HTTP GET with a list of integers in JSON format.
--   * /cats -- Responds to HTTP GET with a list of Strings in JSON format.
type MyAPI = "dogs" :> Get '[JSON] [Int] :<|> "cats" :> Get '[JSON] [String]

-- | A WAI 'Application' that will serve our API.
app :: Application
app = serve (Proxy :: Proxy MyAPI) myAPI

-- | Our entire API.  You can see that it is a combination of the 'dogNums'
-- handler and the 'cats' handler.
myAPI :: ServerT MyAPI (Handler)
myAPI = dogNums :<|> cats

-- | A handler for the /dogs route.  It just returns a list of the integers
-- one to four.
dogNums :: Handler [Int]
dogNums = return [1, 2, 3, 4]

-- | A handler for the /cats route.
cats :: Handler [String]
cats = return ["long-haired", "short-haired"]

-- | Run our 'app' as a WAI 'Application'.
main :: IO ()
main = run 32323 $ logStdoutDev app

a = symbolVal (Proxy :: Proxy "hello")

-- >>> a
-- "hello"
--
