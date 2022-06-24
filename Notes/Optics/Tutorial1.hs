{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Optics.Tutorial1 where

import Control.Applicative
import Data.Kind
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import Data.Functor.Identity
import Data.Functor (($>))

type Lens s t a b = (forall f. Functor f => (a -> f b) -> s -> f t)

type Lens' s a = Lens s s a a

type Getting r s a = (a -> Const r a) -> s -> Const r s

type Setting s t a b = (a -> Identity b) -> s -> Identity t

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

type Traversal' s a = Traversal s s a a

view :: Getting a s a -> s -> a
view lens = getConst . lens Const

set :: Setting s t a b -> b -> s -> t
set lens b = runIdentity . lens (const (Identity b))

over :: Setting s t a b -> (a -> b) -> s -> t
over lens f = runIdentity . lens (Identity . f)

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = flip appEndo [] . getConst . l (\a -> Const (Endo (a :)))

--toListOf' :: Getting [a] s a -> s -> [a]
--toListOf' l = getConst . l (\a -> Const [a])

preview :: Getting (First a) s a -> s -> Maybe a
preview l = getFirst . getConst . l (Const . First . Just)

has :: Getting Any s a -> s -> Bool
has l = getAny . getConst . l (\_ -> Const (Any True))

o ^. l = view l o

(%~) = over

(&) = flip ($)

(.~) = set

(^..) = flip toListOf

(^?) = flip preview

-- >>>  (1, 2) & (_1 %~ negate)
-- >>> (1, 2) &( _1 .~ 88)
-- >>> (1, 2) ^.. each
-- >>>  [(0 :: Int) .. 10] ^? each
-- (-1,2)
-- (88,2)
-- [1,2]
-- Just 0
--

-----------------------------------------------

-- Modify the target of a lens and return the result.
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f = l ((,) <$> f <*> f)

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f = l ((,) <$> id <*> f)

-----------------------------------------------------------------------

to :: (s -> a) -> Getting r s a
to getter f s = Const (getConst (f (getter s)))

-- >>> view (to (!!1)) [1,2,3]
-- >>> toListOf (to (!!1)) [1,2,3]
-- >>> preview (to (!!4)) [1,2,3]
-- 2
-- [2]

-- *** Exception: Prelude.!!: index too large

-- Just
--

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter f s = setter s <$> f (getter s)

_1 :: Lens (a, x) (b, x) a b
_1 = lens fst (\(a, x) b -> (b, x))

_2 :: Lens (x, a) (x, b) a b
_2 = lens snd (\(x, a) b -> (x, b))

choosing :: Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 f = either (fmap Left . l1 f) (fmap Right . l2 f)

-- >>>  set _1 'a' (1, 2)
-- >>>  over _1 show (1, 2)
-- >>>  set (_1 . _1) 'a' ((1, 2), 3)
-- ('a',2)
-- ("1",2)
-- (('a',2),3)
--
--------------------------------------------------------------------------------------
-- each
class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b
  default each :: (Traversable g, s ~ g a, t ~ g b) => Traversal s t a b
  each = traverse

instance (a ~ a', b ~ b') => Each (a, a') (b, b') a b where
  each f ~(a, b) = (,) <$> f a <*> f b

instance Each [a] [b] a b

instance Each (Identity a) (Identity b) a b

instance Each (Maybe a) (Maybe b) a b

-- >>> toListOf each (1, 2)
-- [1,2]
--

------------------------------------------
-- at ix

type family Index (s :: Type) :: Type where
  Index [a] = Int
  Index (M.Map k a) = k
  Index (S.Set a) = a

type family IxValue (m :: Type) :: Type where
  IxValue [a] = a
  IxValue (M.Map k a) = a
  IxValue (S.Set a) = ()

class Ixed m where
  ix :: Index m -> Traversal' m (IxValue m)

instance Ixed [a] where
  ix i f = go i
    where
      go n xs | n < 0 = pure xs
      go _ [] = pure []
      go 0 (a : as) = (: as) <$> f a
      go n (a : as) = (a :) <$> go (n - 1) as

instance (Ord k) => Ixed (M.Map k v) where
  ix k f m = case M.lookup k m of
    Nothing -> pure m
    (Just a) -> (\a' -> M.insert k a' m) <$> f a

instance (Ord k) => Ixed (S.Set k) where
  ix k f m = if S.member k m then f () $> m else pure m

-- >>> [(1 :: Int), 2, 3, 4, 5] & (ix 2 %~ (* 10))
-- >>> [(1 :: Int), 2, 3, 4, 5] & (ix 2 .~ 11)
-- >>> [(1 :: Int), 2, 3, 4, 5] ^? ix 2
-- >>> M.empty ^? ix 2
-- >>> [] ^? ix 2
-- [1,2,30,4,5]
-- [1,2,11,4,5]
-- Just 3
-- Nothing
-- Nothing
--

---
--- At
---
class Ixed m => At m where
  at :: Index m -> Lens' m (Maybe (IxValue m))

instance (Ord k) => At (M.Map k v) where
  --use alterF
  at k f m = go item <$> f item
    where
      item = M.lookup k m
      go Nothing Nothing = m
      go _ Nothing = M.delete k m
      go _ (Just x) = M.insert k x m

instance (Ord k) => At (S.Set k) where
  at k = undefined

--a =

-- >>> (at 1 .~ (Just "hello")) $ M.empty
-- >>> M.fromList [(1,"world")] ^.at 1
-- fromList [(1,"hello")]
-- Just "world"
--

-----------------------------------------------------------------------

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

instance Profunctor (->) where
  dimap pre post f = post . f . pre

newtype Kiesli m a b = Kiesli {runKleisli :: a -> m b}

instance (Functor m) => Profunctor (Kiesli m) where
  dimap pre post f = Kiesli (fmap post . runKleisli f . pre)

newtype Tagged a b = Tagged {unTagged :: b}

instance Profunctor Tagged where
  dimap pre post (Tagged b) = Tagged (post b)

class Profunctor p => Choice p where
  left' :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)

instance Choice (->) where
  left' f = either (Left . f) Right
  right' = fmap

instance (Monad m) => Choice (Kiesli m) where
  left' (Kiesli k) = Kiesli $ \a -> either (fmap Left . k) (return . Right) a
  right' (Kiesli k) = Kiesli $ \a -> either (return . Left) (fmap Right . k) a

instance Choice Tagged where
  left' (Tagged k) = Tagged $ Left k
  right' (Tagged k) = Tagged $ Right k

------------------------------------------------------------------------

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'

-- >>> over _Left (+1) (Left 2)
-- >>> Left "hello" ^._Left
-- >>> over _Left (+1) (Right 2)
-- >>> Right 42 ^._Left :: String
-- Left 3
-- "hello"
-- Right 2
-- ""
--

_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ either Right (Left . Right)

_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (Left . Left) Right