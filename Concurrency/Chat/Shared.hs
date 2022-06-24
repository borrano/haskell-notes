module Chat.Shared where

import Control.Concurrent (MVar, forkFinally, newMVar)
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import Data.Bifunctor
import qualified Data.ByteString as S
import Data.Foldable (traverse_)
import GHC.IO.Handle
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO

hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}

open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
  setSocketOption sock ReuseAddr 1
  withFdSocket sock setCloseOnExecIfNeeded
  bind sock $ addrAddress addr
  listen sock 1024 >> return sock

runTCPServer :: Maybe HostName -> ServiceName -> s -> (s -> Handle -> IO a) -> IO a
runTCPServer mhost port state server = withSocketsDo $ do
  addr <- head <$> getAddrInfo (Just hints) mhost (Just port)
  E.bracket (open addr) close loop
  where
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $ \(conn, _peer) -> do
        handle <- socketToHandle conn ReadWriteMode
        hSetBuffering handle LineBuffering
        forkFinally (server state handle) (\_ -> hClose handle)
