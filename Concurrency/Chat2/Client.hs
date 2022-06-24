{-# LANGUAGE OverloadedStrings #-}

module Chat2.Client where
 
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Char8 qualified as T
import Data.Text
import Network.Socket
import Network.WebSockets qualified as WS
import Chat2.Types

client :: IO ()
client = withSocketsDo $ WS.runClient "localhost" 9160 "/" app
  where
    app :: WS.ClientApp ()
    app conn = do
      putStrLn "Connected!"
      _ <- forkIO $
        forever $ do
          msg <- WS.receiveData conn
          liftIO $ T.putStrLn msg
      let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn (encode $ CBroadcast "" "asdasd") >> loop
      loop
      WS.sendClose conn ("Bye!" :: Text)
