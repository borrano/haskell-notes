{-# LANGUAGE NoMonomorphismRestriction #-}

module Paralelism.Eval where

import Control.DeepSeq
import Control.Parallel.Strategies hiding (Strategy)
import Data.Foldable (traverse_)
import Data.List.Extra (unfoldr)
import Data.Text.Unsafe (iter)
import Data.Tuple
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

-- >>> let x = 1 + 2 :: Int
-- >>> :sprint x
-- >>> x -- x is a thunk
-- >>> :sprint x
-- x = _
-- 3
-- x = 3
--

-- >>> let x = 1 + 2 :: Int
-- >>> let y = x + 1
-- >>> :sprint x
-- >>> :sprint y
-- >>> seq y () -- x is evaluated too
-- >>> :sprint x
-- >>> :sprint y
-- x = _
-- y = _
-- ()
-- x = 3
-- y = 4
--

-- >>> let x = 1 + 3 :: Int
-- >>> let z = (x, x)
-- >>> :sprint z
-- >>> let y = swap z
-- >>> :sprint y
-- >>> seq y () -- only evaluates to whnf
-- >>> :sprint y
-- z = (_,_)
-- y = _
-- ()
-- y = (_,_)
--

-- >>> es = [1..10] :: [Int]
-- >>> es -- evaluate fully
-- >>> let xs = fmap (+1)  es
-- >>> :sprint xs
-- >>> seq xs ()
-- >>> :sprint xs
-- >>> length xs
-- >>> :sprint xs
-- >>> sum xs
-- >>> :sprint xs
-- [1,2,3,4,5,6,7,8,9,10]
-- xs = _
-- ()
-- xs = _ : _
-- 10
-- xs = [_,_,_,_,_,_,_,_,_,_]
-- 65
-- xs = [2,3,4,5,6,7,8,9,10,11]
--
main1 = print $ kmeans  testPoints

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

parFib n = runEval $ do
  a <- rpar $ fib (n - 1)
  b <- rpar $ fib (n - 2)
  return $ a + b

-- >>> parFib 20
-- 6765
--

input :: [Int]
input = take 40 $ cycle [33 .. 38]

parallelFib = runEval $ do
  let (l, r) = splitAt 10 input
  a <- rpar $ force (fmap fib l)
  b <- rpar $ force (fmap fib r)
  return $ a ++ b

parMap' [] = return []
parMap' (x : xs) = (:) <$> rpar x <*> parMap' xs

parallelFib2 = runEval $ parMap' (fmap fib input)

type Strategy a = a -> Eval a

rpar' :: Strategy a
rpar' = rpar

rseq' :: Strategy a
rseq' = rseq

evalPair :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair sa sb (a, b) = (,) <$> sa a <*> sb b

using' :: a -> Strategy a -> a
using' x f = runEval (f x)

rdeepseq' :: NFData a => Strategy a
rdeepseq' a = rseq (force (a))

rparWith' :: Strategy a -> Strategy a
rparWith' sa a = sa a >>= rpar

parPair = evalPair (rparWith' rdeepseq') (rparWith' rdeepseq')

---------------------------------------------------------------------

data Point = Point {-# UNPACK #-} !Double {-# UNPACK #-} !Double deriving (Show)

sumPoint (Point a b) (Point x y) = (Point (a + x) (b + y))

distance :: Point -> Point -> Double
distance (Point x y) (Point a b) = ((x - a) ^ 2) + ((y - b) ^ 2)

oneStep :: V.Vector Point -> V.Vector Point -> V.Vector (Int, Point)
oneStep points clusters = newClusters
  where
    assignment p = V.minIndexBy (\a b -> compare (distance a p) (distance b p)) clusters
    addPoint x (!c, y) = (c + 1, sumPoint x y)
    newClusters = V.create $ do
      vec <- MV.replicate (length clusters) (0, Point 0 0)
      traverse_ (\p -> MV.modify vec (addPoint p) (assignment p)) points
      return vec

sumToCluster :: V.Vector (Int, Point) -> V.Vector Point
sumToCluster = V.map (\(c, (Point x y)) -> Point (x / (fromIntegral c)) (y / (fromIntegral c)))

testPoints = V.fromList [Point x y | x <- [0 .. 1000], y <- [0 .. 1000]]

kmeans xs = head $ drop 20 $ iterate (sumToCluster . oneStep xs) (V.take 3 xs)

kmeansPar xs = head $ drop 20 $ iterate (\c -> sumToCluster $ foldl1 combine $ (fmap (flip oneStep c) chunks `using` parList rseq)) (V.take 3 xs)
  where
    chunks = chunksOf 1000 xs
    combine = V.zipWith (\(!c1, a) (!c2, b) -> (c1 + c2, sumPoint a b))

-- >>> kmeansPar testPoints
-- [(50.0,16.0),(50.0,49.5),(50.0,83.5)]
--
chunksOf :: Int -> V.Vector a -> [V.Vector a]
chunksOf i = unfoldr go
  where
    go v
      | V.null v = Nothing
      | otherwise = Just (V.splitAt i v)