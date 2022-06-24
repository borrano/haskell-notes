-- https://www.well-typed.com/blog/2015/11/implementing-a-minimal-version-of-haskell-servant/
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.Implementation where

import Control.Applicative
import Data.Data
import Data.Time
import GHC.TypeLits
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Text.Read

data Get (a :: *)

data a :<|> b = a :<|> b

infixr 8 :<|>

data (a :: k) :> (b :: *)

infixr 9 :>

data Capture (a :: *)

data JSON

type family Server layout :: *

type instance Server (Get a) = IO a

type instance Server (a :<|> b) = Server a :<|> Server b

type instance Server ((s :: Symbol) :> r) = Server r

type instance Server (Capture a :> r) = a -> Server r

class HasServer layout where
  route :: Proxy layout -> Server layout -> [String] -> Maybe (IO String)

instance Show a => HasServer (Get a) where
  route :: Proxy (Get a) -> IO a -> [String] -> Maybe (IO String)
  route _ handler [] = Just (show <$> handler)
  route _ _ _ = Nothing

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route :: Proxy (a :<|> b) -> (Server a :<|> Server b) -> [String] -> Maybe (IO String)
  route _ (handlera :<|> handlerb) xs =
    route (Proxy :: Proxy a) handlera xs <|> route (Proxy :: Proxy b) handlerb xs

instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  route :: Proxy (s :> r) -> Server r -> [String] -> Maybe (IO String)
  route _ handler (x : xs)
    | symbolVal (Proxy :: Proxy s) == x =
        route (Proxy :: Proxy r) handler xs
  route _ _ _ = Nothing

instance (Read a, HasServer r) => HasServer (Capture a :> r) where
  route :: Proxy (Capture a :> r) -> (a -> Server r) -> [String] -> Maybe (IO String)
  route _ handler (x : xs) = do
    a <- readMaybe x
    route (Proxy :: Proxy r) (handler a) xs
  route _ _ _ = Nothing

serve :: HasServer layout => Proxy layout -> Server layout -> [String] -> IO String
serve p h xs = case route p h xs of
  Nothing -> ioError (userError "404")
  Just m -> m

-- >>> serve (Proxy :: Proxy MyAPI) handleMyAPI ["time", "UTC"]
-- "2022-04-13 16:51:15.278632102 UTC"
--

-- >>> serve   (Proxy :: Proxy MyAPI) handleMyAPI ["date"]
-- "2022-04-13"
--

example = serve (Proxy :: Proxy MyAPI) handleMyAPI ["date"]

type MyAPI = "date" :> Get Day :<|> "time" :> Capture TimeZone :> Get ZonedTime

handleDate :: IO Day
handleDate = utctDay <$> getCurrentTime

handleTime :: TimeZone -> IO ZonedTime
handleTime tz = utcToZonedTime tz <$> getCurrentTime

handleMyAPI :: Server MyAPI
handleMyAPI = handleDate :<|> handleTime

x :: Maybe (TimeZone)
x = readMaybe "UTC"

-- (Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived)
test :: IO ()
test = run 8888 (\req cont -> undefined)

-- >>> x
-- Just UTC
--
