{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Free.Tutorial2 where

import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor (($>))
import qualified Data.Map as M

-- https://drive.google.com/file/d/1hOEQu8l-3Lv4vr1sTv3gvJBUpumQEu5Y/view?usp=sharing
data Expr = Lit Int | Add Expr Expr | Div Expr Expr | Var String | Assign String Expr | Seq Expr Expr

---- mtl
--newtype Eval a = Eval {runEval :: StateT (M.Map String Int) (Either String) a} deriving (Functor, Applicative, Monad, MonadError String, MonadState (M.Map String Int))
--
----
--divZero :: Eval a
--divZero = throwError "div by zero"
--
--getVar :: String -> Eval Int
--getVar str = gets (M.lookup str) >>= maybe (throwError "no var") return
--
--setVar :: String -> Int -> Eval ()
--setVar str v = modify (M.insert str v)

data EvalF r where
  DivZero :: EvalF r
  SetVar :: String -> Int -> (Int -> r) -> EvalF r
  GetVar :: String -> (Int -> r) -> EvalF r
  deriving (Functor)

type Eval a = Free EvalF a

divZero = Free DivZero

getVar :: String -> Eval Int
getVar str = Free $ GetVar str Pure

setVar :: String -> Int -> Free EvalF Int
setVar str v = Free $ SetVar str v Pure

eval :: Expr -> Eval Int
eval (Lit i) = return i
eval (Var s) = getVar s
eval (Assign s v) = eval v >>= (\a -> setVar s a >> pure a)
eval (Seq a b) = eval a >> eval b
eval (Add a b) = (+) <$> eval a <*> eval b
eval (Div a b) = do
  b' <- eval b
  if b' == 0 then divZero else (`div` b') <$> eval a

foldFree' :: Eval Int -> StateT (M.Map String Int) (Either String) Int
foldFree' (Free (DivZero)) = throwError "divZero"
foldFree' (Free (SetVar str v rest)) = modify (M.insert str v) >> pure v
foldFree' (Free (GetVar str rest)) = gets (M.lookup str) >>= maybe (throwError "no var") return
foldFree' (Pure a) = return a

testE = flip runStateT (M.empty) $ foldFree' $ eval $ Seq (Assign "a" (Lit 2)) (Var "a")

-- >>> testE
-- Right (2,fromList [("a",2)])
