module Chat.Factor1 where

import Chat.Shared
import Control.Concurrent (MVar, forkFinally, newMVar)
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import Data.Bifunctor
import qualified Data.ByteString as S
import Data.Foldable (traverse_)
import GHC.IO.Handle
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO

-- However, we have to be careful. If multiple threads write to a Handle simultaneously, the messages might get
-- interleaved in an arbitrary way. To make sure messages don’t get interleaved, we can use the MVar as a lock.
-- But this means that every server thread, when it needs to send a message to its client, must hold the MVar while sending the message.

newtype State = State (MVar ([Handle], Integer))

newState :: IO State
newState = State <$> newMVar ([], 10)

addClient :: State -> Handle -> IO ()
addClient (State state) h = modifyMVar_ state (return . first (h :))

removeClient (State state) h = modifyMVar_ state (return . first (filter (/= h)))

changeFactor :: State -> Integer -> IO ()
changeFactor (State state) num = modifyMVar_ state $ \(xs, _) -> do
  traverse_ (\h -> hPrint h ("new factor is " ++ show num)) xs
  return (xs, num)

readFactor :: State -> IO Integer
readFactor (State state) = snd <$> readMVar state

startChat :: IO ()
startChat = do
  state <- newState
  runTCPServer Nothing "4444" state loop

loop :: State -> Handle -> IO ()
loop s h = E.bracket (addClient s h) (\_ -> removeClient s h) (const go)
  where
    go = do
      line <- hGetLine h
      case line of
        "end" -> hPrint h $ "Thank you for using the " ++ "Haskell doubling service."
        [] -> go
        ('=' : num) -> changeFactor s (read num) >> go
        num -> (readFactor s >>= \n -> hPrint h (n * read num)) >> go
