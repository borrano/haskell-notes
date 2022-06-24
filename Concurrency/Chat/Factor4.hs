module Chat.Factor4 where

import Chat.Shared (runTCPServer)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import GHC.IO.Handle
import System.IO (hPrint)

newtype State = State (TVar Int)

newState :: IO State
newState = State <$> newTVarIO 10

startChat :: IO ()
startChat = do
  state <- newState
  runTCPServer Nothing "4444" state loop

data Msg = End | M Int

loop :: State -> Handle -> IO ()
loop (State s) h = do
  num <- readTVarIO s
  c <- atomically newTChan
  void $ race (go c) (process c num)
  where
    go c = do
      line <- hGetLine h
      case line of
        "end" -> atomically $ writeTChan c End
        [] -> go c
        ('=' : num) -> atomically (writeTVar s (read num)) >> go c
        num -> atomically (writeTChan c (M (read num))) >> go c
    process c n = join $
      atomically $ do
        n' <- readTVar s
        if n /= n'
          then return (hPrint h ("factor is" ++ show n') >> process c n')
          else do
            msg <- readTChan c
            case msg of
              End -> return $ hPrint h "ended"
              (M i) -> return $ (hPrint h (show (i * n)) >> process c n)