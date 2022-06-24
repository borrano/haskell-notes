{-# LANGUAGE DeriveFunctor #-}

module Streaming.Tutorial1 where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char (toUpper)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Void (Void)
import GHC.IO (unsafeInterleaveIO, unsafePerformIO)
import GHC.IO.Handle
import GHC.IO.IOMode
import System.IO

--https://skillsmatter.com/skillscasts/4230-lazy-io-and-alternatives-in-haskell
------------------------------------------------
-- normal i/o vs lazy i/o

process = do
  withFile "" ReadMode $ \inFile ->
    withFile "" ReadMode $
      \outFile -> go inFile outFile
  where
    go i o = do
      eof <- hIsEOF i
      unless eof $ (hGetLine i >>= (hPutStrLn o . fmap toUpper)) >> go i o

processLazy = do
  str <- readFile ""
  writeFile "" (toUpper <$> str)

-----------------------------------------
-- unsafeInterleaveIO

global :: IORef Integer
{-# NOINLINE global #-}
global = unsafePerformIO $ newIORef 0

test = do
  x <- unsafeInterleaveIO (readIORef global)
  y <- unsafeInterleaveIO (writeIORef global 1 >> return 12)
  return (y + x) -- 12
  -- return (x + y) -- 13
  -- >>> test
  -- 12
  --
  -----------------------------------------------------

newtype Pipe i o r = Pipe {getPipe :: IO (PipeStep i o r)} deriving (Functor)

data PipeStep i o r
  = Pure r
  | Request (i -> Pipe i o r)
  | Respond o (Pipe i o r)
  deriving (Functor)

instance Applicative (Pipe i o) where
  pure = Pipe . return . Pure
  (<*>) = ap

instance Monad (Pipe i o) where
  (Pipe io) >>= f = Pipe $ do
    x <- io
    case x of
      (Pure r) -> getPipe $ f r
      (Request g) -> return $ Request $ (g >=> f)
      (Respond o r) -> return $ Respond o (r >>= f)

instance MonadIO (Pipe i o) where
  liftIO io = Pipe $ Pure <$> io

runPipe1 :: IO i -> (o -> IO ()) -> Pipe i o r -> IO r
runPipe1 i o p = do
  x <- getPipe p
  case x of
    (Pure r) -> return r
    (Request f) -> do
      x <- i
      runPipe1 i o $ f x
    (Respond a b) -> do
      o a
      runPipe1 i o b

request :: Pipe i a i
request = Pipe $ return (Request return)

respond :: o -> Pipe i o ()
respond b = Pipe $ return (Respond b (return ()))

mapPipe f = request >>= (respond . f)

runPipe :: Pipe () Void b -> IO b
runPipe (Pipe a) = do
  i <- a
  case i of
    (Pure x) -> return x
    (Request x) -> runPipe $ x ()
    (Respond item y) -> undefined

(.|) = goRight
  where
    goRight l r = Pipe $ do
      step <- getPipe r
      case step of
        (Pure x) -> return $ Pure x
        (Request x) -> getPipe $ goLeft l x
        (Respond item y) -> return $ Respond item (goRight l y)
    goLeft l r = Pipe $ do
      step <- getPipe l
      case step of
        (Pure x) -> return $ Pure x
        (Request f) -> return $ Request (\x' -> goLeft (f x') r)
        (Respond item n) -> getPipe $ goRight n (r item)

program = runPipe (producer .| takePipe 3 .| consumer)

takePipe i = replicateM_ i (request >>= respond)

-- >>> program
producer :: Pipe i String b
producer = do
  liftIO $ print "boran"
  forever $ liftIO getLine >>= respond

consumer :: Pipe String b r
consumer = forever $ (liftIO $ print "xx") >> (request >>= (liftIO . print))

--------------------------------------
-- finalizers https://skillsmatter.com/skillscasts/4230-lazy-io-and-alternatives-in-haskell 57.00 minute mark wierd implementation
-- add finalizer io action
type Finalizer = IO ()

finallyP = undefined

------------------------------------------
-- exceptions -- easy 
-- async exception mask unmask