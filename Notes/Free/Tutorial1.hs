{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Free.Tutorial1 where

import Control.Applicative (Const)
import Control.Monad
import Data.Void
import Optics.Lib (x)

----------------------------------------------------------------------------------

-- https://skillsmatter.com/skillscasts/4430-monads-for-free

-- Deep embedding
-- embed everything as data

data Expr = Lit Int | Add Expr Expr

construct = Add (Lit 2) (Lit 3)

eval1 (Lit a) = a
eval1 (Add a b) = eval1 a + eval1 b

eval2 (Lit a) = show a
eval2 (Add a b) = eval2 a ++ "+" ++ eval2 b

------------------------------------
-- a language with imperative flavour

say1 = print

ask1 = getLine

prog1 = do
  say1 "asd"
  a <- ask1
  say1 a

-- deep embadding

data Interaction2 a where
  Say2 :: String -> Interaction2 ()
  Ask2 :: Interaction2 String
  Return2 :: a -> Interaction2 a
  Bind2 :: Interaction2 a -> (a -> Interaction2 b) -> Interaction2 b

say2 = Say2

ask2 = Ask2

instance Functor Interaction2 where
  fmap = liftM

instance Applicative Interaction2 where
  pure = Return2
  (<*>) = ap

instance Monad Interaction2 where
  (>>=) = Bind2

prog2 = do
  say2 "boran"
  x <- ask2
  say2 x

--- associativity laws are broken

say' :: String -> (() -> Interaction2 b) -> Interaction2 b
say' x = Bind2 $ Say2 x

ask' :: (String -> Interaction2 b) -> Interaction2 b
ask' = Bind2 Ask2

data Interaction3 a where
  Return3 :: a -> Interaction3 a
  Say3 :: String -> (() -> Interaction3 b) -> Interaction3 b --  (() -> Interaction3 b)  =  (  Interaction3 b)
  Ask3 :: (String -> Interaction3 b) -> Interaction3 b

instance Functor Interaction3 where
  fmap = liftM

instance Applicative Interaction3 where
  pure = Return3
  (<*>) = ap

instance Monad Interaction3 where
  (Return3 a) >>= f = f a
  (Say3 a f) >>= g = Say3 a (f >=> g)
  (Ask3 f) >>= g = Ask3 (f >=> g)

say3 a = Say3 a Return3

ask3 = Ask3 Return3

prog3 = do
  say3 "asd"
  a <- ask3
  say3 "s"

run3 (Return3 x) = return x
run3 (Say3 a f) = print a >>= run3 . f
run3 (Ask3 f) = getLine >>= run3 . f

data Interaction4 a where
  Return4 :: a -> Interaction4 a
  Wrap4 :: InteractionOp4 a -> Interaction4 a

data InteractionOp4 a where
  Say4 :: String -> (() -> Interaction4 b) -> InteractionOp4 b --  (() -> Interaction3 b)  =  (  Interaction3 b)
  Ask4 :: (String -> Interaction4 b) -> InteractionOp4 b

data Interaction5 a where
  Return5 :: a -> Interaction5 a
  Wrap5 :: InteractionOp5 (InteractionOp5 a) -> Interaction5 a

data InteractionOp5 r where
  Say5 :: String -> (() -> r) -> InteractionOp5 r
  Ask5 :: (String -> r) -> InteractionOp5 r

--------------------------------------------------------------------

data Free f a where
  Pure :: a -> Free f a
  Free :: f (Free f a) -> Free f a

data InteractionOp6 r where
  Say6 :: String -> (() -> r) -> InteractionOp6 r
  Ask6 :: (String -> r) -> InteractionOp6 r

type Interaction6 a = Free InteractionOp6

--- free is a monad if f is a functor

instance Functor f => Functor (Free f) where
  fmap = liftM

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  (Pure a) >>= f = f a
  (Free f) >>= g = Free (fmap (>>= g) f)

instance Functor InteractionOp6 where
  fmap f (Say6 a g) = Say6 a (f . g)
  fmap f (Ask6 g) = Ask6 (f . g)

type Identity = Free (Const Void)

type Maybe' = Free (Const ())

------------------------------------------------------------------
data ProcessF r where
  Atomically :: IO a -> (a -> r) -> ProcessF r
  Fork :: Process () -> r -> ProcessF r

deriving instance Functor ProcessF

type Process = Free ProcessF

atomically io = Free $ Atomically io Pure

fork p = Free $ Fork p (Pure ())

schedule :: [Process ()] -> IO ()
schedule [] = return ()
schedule (Free (Atomically io f) : xs) = do
  x <- io
  schedule (xs ++ [f x])
schedule (Free (Fork p r) : xs) = schedule (xs ++ [r, p])
schedule (Pure a : xs) = schedule xs

testschedule = schedule $
  pure $ do
    fork $ replicateM_ 5 (atomically (print "boran"))
    fork $ replicateM_ 5 (atomically (print "duygus"))
    atomically (print "x")

-- >>> testschedule
-- "boran"
-- "x"
-- "duygus"
-- "boran"
-- "duygus"
-- "boran"
-- "duygus"
-- "boran"
-- "duygus"
-- "boran"
-- "duygus"
--
