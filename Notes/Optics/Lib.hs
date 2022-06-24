module Optics.Lib where

import Control.Lens
import Data.List

x = view (to (!! 3)) [1, 2, 3]

-- >>> x

--

lt xs = zipWith3 go [0 ..] xs r
  where
    r = transpose $ map reverse xs
    l = length xs
    go i as bs = take (l - i - 1) as ++ drop i bs

-- >>> rotl [[1,2,3],[0,4,5],[0,0,6]]
-- [[3,5,6],[2,4,0],[1,0,0]]
--

-- >>> lt  [[1,2,3],[4,5,6],[7,8,9]]
-- [[1,2,3,6,9],[4,5,8],[7]]
--

lShapedTraverse' xss = go (length xss - 2) xss
  where
    go n [] = ([], [])
    go n (xs : xss) =
      let (l, r) = go (n - 1) xss
          (a, b) = splitAt n xs
       in (a : l, b : r)

lShapedTraverse :: [[a]] -> [[a]]
lShapedTraverse [] = []
lShapedTraverse [xs] = [xs]
lShapedTraverse (xs : xss) =
  let (rest, col) = (map init xss, map last xss)
   in (xs ++ col) : lShapedTraverse rest

test = fmap sum $ lShapedTraverse $ replicate 100 [(0 :: Int) .. 99]
