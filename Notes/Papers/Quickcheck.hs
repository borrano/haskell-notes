{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf

module Papers.Quickcheck where

import Data.List (sort)
import System.Process
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen.Unsafe (promote)
import Test.QuickCheck.Monadic

prop1 :: Int -> Int -> Bool
prop1 a b = (a + b) ^ 2 == a ^ 2 + b ^ 2 + 2 * a * b

-- >>> quickCheck prop1
-- +++ OK, passed 100 tests.
--

------------------------------------------------------------------
-- sample, sample'

xs :: IO [Int]
xs = sample' (arbitrary :: Gen Int)

xs' = sample' arbitrary

xs'' :: IO Int
xs'' = generate arbitrary

-- >>> xs
-- >>> xs'
-- >>> xs''
-- [0,-2,-2,3,8,-2,-6,-2,1,15,-10]
-- [(),(),(),(),(),(),(),(),(),(),()]
-- -22
--

------------------------------------------------------------------
-- Generators - elements, oneof, frequency, listOf, vectorOf, choose
-- create generator
elements' :: Gen Char
elements' = elements "abcd"

-- >>> sample' elements'
-- "aadabcbcbda"
--
choose' :: Gen Char
choose' = choose ('a', 'z')

-- >>> sample' choose'
-- "jrxmlztsism"
--
constant = pure 'a'

-- combinators
oneof' = oneof [elements "boran", elements "ekim"]

-- >>> sample' oneof'
-- "ibmniakkebn"
--

frequency' = frequency [(2, elements "boran"), (10, elements "ekim")]

-- >>> sample' frequency'
-- "miemibmakmk"
--

listOf' = listOf frequency'

-- >>> sample' listOf'
-- ["","ko","e","eik","","iemi","ikrikirkknb","im","ikoamekkm","kekkeee","aeeene"]
--

vectorOf' = vectorOf 2 frequency'

-- >>> sample' vectorOf'
-- ["em","rm","mi","em","in","ri","ke","ib","km","im","kk"]
--

-------------------------------------------------------------------------
-- using combinators to write generators

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Enum, Bounded)

data Rank = Num Int | J | Q | K | A deriving (Show)

data Card = Card Suit Rank deriving (Show)

genEnum :: forall a. (Bounded a, Enum a) => Gen a
genEnum = toEnum <$> choose (fromEnum (minBound :: a), fromEnum (maxBound :: a))

genRank :: Gen Rank
genRank = frequency [(9, num), (4, elements [J, Q, K, A])]
  where
    num = Num <$> elements [2 .. 10]

genSuit :: Gen Suit
genSuit = genEnum

genCard :: Gen Card
genCard = Card <$> genSuit <*> genRank

-- >>> sample' genCard

data Hand = One Card | More Card Hand deriving (Show)

gHand = oneof [One <$> genCard, More <$> genCard <*> gHand]

-- >>> sample' gHand
-- [More (Card Hearts (Num 3)) (More (Card Clubs (Num 5)) (One (Card Diamonds (Num 6)))),One (Card Hearts (Num 3)),More (Card Hearts K) (One (Card Clubs A)),More (Card Clubs (Num 6)) (More (Card Spades (Num 2)) (One (Card Diamonds (Num 2)))),More (Card Clubs (Num 4)) (More (Card Spades (Num 6)) (One (Card Hearts (Num 3)))),More (Card Diamonds Q) (More (Card Diamonds Q) (One (Card Spades A))),More (Card Hearts (Num 10)) (More (Card Diamonds (Num 4)) (More (Card Spades (Num 7)) (More (Card Spades A) (One (Card Hearts (Num 10)))))),One (Card Spades K),More (Card Clubs A) (One (Card Clubs K)),More (Card Spades K) (More (Card Spades (Num 7)) (One (Card Spades (Num 4)))),One (Card Hearts Q)]
--

instance Arbitrary Rank where
  arbitrary = genRank

instance Arbitrary Hand where
  arbitrary = gHand

------------------------------------------------------------------------
-- forAll - specify generator

validRank (Num x) = x >= 2 && x <= 10
validRank _ = True

prop2 = forAll genRank validRank

-- >>> quickCheck prop2
-- +++ OK, passed 100 tests.
--
-- collect: gives distribution
prop3 r = collect r (validRank r)

prop4 :: Hand -> Property
prop4 h = collect h True

prop5 :: Hand -> Property
prop5 h = collect (size h) True
  where
    size (One _) = 1
    size (More _ r) = 1 + size r

-- >>> quickCheck prop5
-- +++ OK, passed 100 tests:
-- 51% 1
-- 24% 2
-- 10% 3
--  6% 4
--  5% 5
--  3% 6
--  1% 9
--

-- >>> quickCheck prop4
-- +++ OK, passed 100 tests:
-- 15% Num 2
-- 12% K
-- 10% Num 8
--  8% Num 4
--  8% Num 9
--  8% Q
--  7% Num 3
--  7% Num 6
--  7% Num 7
--  5% A
--  5% J
--  4% Num 10
--  4% Num 5
--

-- precondition ==>

isOrdered xs = and $ zipWith (<=) xs (tail xs)

-- >>> insert 0 [0, 1]
-- >>> isOrdered[0,0,1]
-- [0,0,1]
-- True
--
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

propInsertOrder :: Int -> [Int] -> Property
propInsertOrder x xs = isOrdered xs ==> isOrdered (insert x xs)

-- >>> quickCheck propInsertOrder

-- *** Gave up! Passed only 74 tests; 1000 discarded tests.

orderedList' = sort <$> arbitrary

orderedList'' = genList minBound
  where
    genList mi = oneof [pure [], choose (mi, maxBound) >>= (\x -> (x :) <$> genList x)]

propInsertOrder2 :: Int -> Property
propInsertOrder2 x = forAll orderedList'' (isOrdered . insert x)

-- >>> quickCheck propInsertOrder2
-- +++ OK, passed 100 tests.
--
propInsertOrder3 :: Int -> OrderedList Int -> Bool
propInsertOrder3 x xs = isOrdered $ insert x (getOrdered xs)

-- >>> quickCheck propInsertOrder3
-- +++ OK, passed 100 tests.
--

-- cover: Checks that at least the given proportion of successful test cases belong to the given class.
-- Discarded tests (i.e. ones with a false precondition) do not affect coverage.
cover' x xs = cover 1 (length (getOrdered xs) > 10) "long lists" (isOrdered $ insert x (getOrdered xs))

-- >>> quickCheck cover'
-- +++ OK, passed 100 tests (65% long lists).
--
prop_sorted_sort :: Property
prop_sorted_sort = forAll (orderedList :: Gen [Int]) $ \xs ->
  classify (length xs > 12) "non-trivial" $
    sort xs === xs

-- >>> quickCheck prop_sorted_sort
-- +++ OK, passed 100 tests (68% non-trivial).
--
--

--------------------------------------------------------------------------------------
-- sized
-- We’ll tie the size to the likelihood of continuing to build a list and use the sized function to read the parameter
sizedList = sized $ \n -> frequency [(1, pure []), (n, (:) <$> arbitrary <*> sizedList)]

resize' = resize 100 (sizedList :: Gen [Int])

scale' = scale (* 33) (sizedList :: Gen [Int])

-- >>> length <$> generate (resize')
-- 89
--
-- >>> length <$>  generate (scale')
-- 73
--
-- quickCheckAll

runTests :: IO Bool
runTests =
  $forAllProperties $
    quickCheckWithResult (stdArgs {maxSuccess = 10000})

-- >>> runTests
-- True
--

--------------------------------------------------------------------------------------
-- testing io/monadic actions

-- $ factor 16
-- 16: 2 2 2 2

factor :: Integer -> IO [Integer]
factor n = parse `fmap` readProcess "factor" [show n] ""
  where
    parse :: String -> [Integer]
    parse = map read . tail . words

prop_factor :: Property
prop_factor =
  monadicIO $
    forAllM arbitrary $ \n -> do
      pre (n > 13)
      factors <- run (factor n)
      assert (product factors == n)

-- >>> quickCheck prop_factor

------------------------------------------------------------------------
-- function generation

propf :: Fun String Integer -> Bool
propf (Fun _ f) = f "monkey" == f "banana" || f "banana" == f "elephant"

-- >>> quickCheck propf

-- *** Failed! Falsified (after 2 tests and 75 shrinks):

-- {"banana"->0, _->1}
--
