module Chat.Factor3 where

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

newtype State = State (Chan Int)

newState :: IO State
newState = State <$> newChan

addClient (State state) = do
  c <- newChan
  broadCastChan <- dupChan state
  return (c, broadCastChan)

startChat :: IO ()
startChat = do
  state <- newState
  runTCPServer Nothing "4444" state loop

loop :: State -> Handle -> IO ()
loop s h = do
  (c, b) <- addClient s
  void $ race (race (go c b) (process 10 c)) (forever $ (readChan b >>= (writeChan c . NewFactor)))
  where
    go c b = do
      line <- hGetLine h
      case line of
        "end" -> hPrint h $ "Thank you for using the " ++ "Haskell doubling service."
        [] -> go c b
        ('=' : num) -> writeChan b (read num) >> go c b
        num -> writeChan c (ClientMsg (read num)) >> go c b
    process n c = do
      msg <- readChan c
      case msg of
        (ClientMsg m) -> hPrint h (n * m) >> process n c
        (NewFactor f) -> hPrint h ("new factor is" ++ show f) >> process f c
