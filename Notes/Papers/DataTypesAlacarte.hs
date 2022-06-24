{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Papers.DataTypesAlacarte where

data Expr0 = Num0 Int | Add0 Expr0 Expr0

-- make it non recursive
data Expr1F a = Num1 Int | Add1 a a

instance Functor Expr1F where
  fmap f (Add1 a b) = Add1 (f a) (f b)
  fmap f (Num1 a) = Num1 a

newtype Fix f = Fix {out :: f (Fix f)}

type Expr1 = Fix Expr1F

type Algebra f a = f a -> a

foldMu :: (Functor f) => Algebra f a -> Fix f -> a
foldMu alg = alg . fmap (foldMu alg) . out

data Val e = Val Int

data Add e = Add e e

data Mul e = Mul e e

-------

data (f :+: g) e = L (f e) | R (g e) deriving (Functor)

type AddVal = (Add :+: Val)

k :: Fix AddVal
k = Fix $ L (Add (Fix $ R $ Val 2) (Fix $ R $ Val 2))

class Eval e where
  eval :: e -> Int

instance Eval (Val a) where
  eval (Val a) = a

instance Eval (Add Int) where
  eval (Add a b) = a + b

instance Eval (Mul Int) where
  eval (Mul a b) = a * b