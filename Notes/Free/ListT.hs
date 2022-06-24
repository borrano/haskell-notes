{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Free.ListT where

import Control.Monad.IO.Class
import Debug.Trace
import System.IO

-- https://hackage.haskell.org/package/list-transformer-1.0.7/docs/List-Transformer.html
newtype ListT m a = ListT {next :: m (Step m a)}

data Step m a = Cons a (ListT m a) | Nil deriving (Functor)

instance (Monad m) => Functor (ListT m) where
  fmap f xs = ListT $ (fmap f <$> next xs)

e = ListT $ return Nil

cons :: Monad m => a -> ListT m a -> ListT m a
cons a xs = ListT $ return (Cons a xs)

instance (Monad m) => Applicative (ListT m) where
  pure a = cons a e
  (ListT a) <*> (ListT b) = undefined

instance (Monad m) => Monad (ListT m) where
  xs >>= f = ListT $ do
    i <- next xs
    case i of
      Nil -> return Nil
      (Cons a rest) -> next $ append (f a) (rest >>= f)

instance (MonadIO m) => MonadIO (ListT m) where
  liftIO io = ListT $ (`Cons` e) <$> liftIO io

append :: Monad m => ListT m a -> ListT m a -> ListT m a
append xs ys = ListT $ do
  a <- next xs
  case a of
    Nil -> next ys
    (Cons a rest) -> return (Cons a (append rest ys))

readLine handle = ListT $ do
  b <- hIsEOF handle
  if b
    then return Nil
    else hGetLine handle >>= (\a -> return $ Cons a (readLine handle))

takeM :: (Monad m) => Int -> ListT m a -> ListT m a
takeM = go
  where
    go 0 xs = e
    go n xs = ListT $ do
      a <- next xs
      case a of
        Nil -> return Nil
        (Cons a rest) -> return $ Cons a (go (n - 1) rest)

writeLine listT = runListT $ do
  elem <- listT
  liftIO $ print elem

test1 :: IO ()
test1 = openFile "./x.txt" ReadMode >>= (writeLine . takeM 3 . readLine)

runListT (ListT m) = do
  s <- m
  case s of
    Nil -> return ()
    Cons _ l' -> runListT l'

-- >>> test1
--
