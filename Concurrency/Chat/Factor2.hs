module Chat.Factor2 where

import Chat.Shared (runTCPServer)
import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import GHC.IO.Handle
import System.IO (hPrint)

data Message = NewFactor Int | ClientMsg Int

newtype State = State (MVar [Chan Message])

newState :: IO State
newState = State <$> newMVar []

addClient :: State -> IO (Chan Message)
addClient (State state) = do
  c <- newChan
  modifyMVar_ state (return . (c :))
  return c

removeClient :: State -> Chan Message -> IO ()
removeClient (State state) chan = modifyMVar_ state (return . filter (/= chan))

startChat :: IO ()
startChat = do
  state <- newState
  runTCPServer Nothing "4444" state loop

changeFactor :: State -> Int -> IO ()
changeFactor (State state) num = modifyMVar_ state $ \xs -> do
  traverse_ (\c -> writeChan c (NewFactor num)) xs
  return xs

loop :: State -> Handle -> IO ()
loop s h = do
  E.bracket (addClient s) (removeClient s) (\c -> void $ race (go c) (process 10 c))
  where
    go c = do
      line <- hGetLine h
      case line of
        "end" -> hPrint h $ "Thank you for using the " ++ "Haskell doubling service."
        [] -> go c
        ('=' : num) -> changeFactor s (read num) >> go c
        num -> writeChan c (ClientMsg (read num)) >> go c
    process n c = do
      msg <- readChan c
      case msg of
        (ClientMsg m) -> hPrint h (n * m) >> process n c
        (NewFactor f) -> hPrint h ("new factor is" ++ show f) >> process f c