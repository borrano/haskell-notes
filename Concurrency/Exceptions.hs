{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Exceptions where

import Control.Concurrent (newMVar, threadDelay)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Except (MonadError (catchError))
import Data.Data
import GHC.Base (IO (IO), RealWorld, State#, catch#, raise#, raiseIO#, unIO)
import GHC.Conc
import GHC.IO hiding (onException)

---- imprecise exceptions

a1 = error "boran" + error "duygus"

-- >>> x

a2 :: IO Int
a2 = do
  let a = a1
  return 21

-- >>> ex2
-- 21
--

data MyExceptionBase = forall e. Exception e => MyExceptionBase e deriving (Typeable)

instance Show MyExceptionBase where
  show (MyExceptionBase a) = "base ---- " ++ show a

instance Exception MyExceptionBase where
  toException = SomeException
  fromException (SomeException e) = cast e

newtype MyException = MyException String deriving (Show, Typeable)

instance Exception MyException where
  toException = toException . MyExceptionBase
  fromException e = fromException e >>= (\(MyExceptionBase e') -> cast e')

b1 = throwIO (MyException "asd") `catch` (\(e :: SomeException) -> print e)

b2 = throwIO (MyException "asd") `catch` (\(e :: MyException) -> print e)

b3 = throwIO (MyException "asd") `catch` (\(e :: MyExceptionBase) -> print e)

-- >>> b2
-- MyException "asd"
--

-----------evaluation vs execution

c1 = throw (MyException "boran") `seq` 2 -- evaluation

c2 = throwIO (MyException "boran") `seq` 2 -- execution
-- >>> a1

-- *** Exception: base ---- MyException "boran"

--
-------------------------------
-- implementation
throwIO' :: Exception e => e -> IO a
throwIO' e = IO (raiseIO# (toException e))

throw' :: Exception e => e -> a
throw' e = raise# (toException e)

catch' io f = IO $ catch# (unIO io) handler
  where
    handler e = case fromException e of
      Nothing -> raiseIO# e
      (Just e') -> unIO $ f e'

--throwIO' :: (Exception e) => e -> IO ()
--throwIO' e = raiseIO# e

------------------------------------
-- async exceptions

d1 = do
  tid <- uninterruptibleMask_ $ forkIO $ (threadDelay 30000000)
  killThread tid -- kill thread waits exception to be received
  print "killed"

d2 = do
  -- "killed"
  --
  mvar <- newMVar 12
  tid <- forkIO $ takeMVar mvar >>= (\v -> threadDelay 30000000 >> putMVar mvar v)
  threadDelay 10000000
  killThread tid
  takeMVar mvar >>= print

-- thread blocked indefinitely in an MVar operation - mask async exceptions or use readMVar vs

--- nested mask unmask

d3 = mask $ \unmask -> unmask getMaskingState

d4 = mask $ \_ -> getMaskingState

d5 = mask $ \_ -> mask $ \unmask -> unmask getMaskingState

d6 = mask $ \unmask -> mask $ \_ -> unmask getMaskingState

d7 = mask $ \unmask -> unmask $ mask $ \unmask -> unmask getMaskingState

-- >>> traverse id [d3, d4, d5, d6, d7]
-- [Unmasked,MaskedInterruptible,MaskedInterruptible,Unmasked,Unmasked]
--

--mask' :: ((forall a. IO a -> IO a) -> IO b) -> IO b
--mask' io = do
--  b <- getMaskingState
--  case b of
--    Unmasked -> block  $ io unblock
--    MaskedInterruptible -> io block
--    MaskedUninterruptible -> io blockUninterruptible

----------------------
-- combinators

try' io = (Right <$> io) `catch` (return . Left)

handle' :: Exception e => (e -> IO a) -> IO a -> IO a
handle' = flip catch

onException' io f = io `catch` (\(e :: SomeException) -> f >> throwIO e)

-- before should perform only one blocking operation. An exception raised by a second blocking operation would not result in after being executed
-- Blocking operations in after is interruptible and might receive an asynchronous exception.

bracket' before after io = mask $ \restore -> do
  r <- before
  x <- restore (io r) `onException` after r
  after r
  return x

finally' io after = do
  x <- io `onException` after
  after
  return x