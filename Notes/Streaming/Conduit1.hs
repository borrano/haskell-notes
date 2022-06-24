{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Streaming.Conduit1 where

import Conduit
import Control.Monad
import Data.ByteString (ByteString)
import Data.Foldable
import Data.Text (Text)

-- https://www.youtube.com/watch?v=9957qVltU00&ab_channel=LambdaConf
ex1 = runConduitPure $ yieldMany [1 .. 10] .| sumC

-- >>> ex1
-- 55

ex2 = do
  writeFile "x" "asd"
  runConduitRes $ sourceFile "x" .| sinkFile "y"

-- >>> ex2

ex3 = runConduitPure $ yieldMany [1 .. 10] .| mapC (+ 1) .| sinkList

-- >>> ex3
-- [2,3,4,5,6,7,8,9,10,11]

-- foo .| bar .| baz = foo .| (bar .| baz) = (foo .| bar) .| baz
-- foo is upstream of bar
-- bar is upstream of baz
-- bar awaits foo - foo yields to bar
-- start at downstream - awaits a value from upstream  - control is in downstream if downstream never awaits upstream is never executed

x :: ConduitT () Integer Identity ()
x = yieldMany [1 .. 10]

y :: ConduitT Integer Integer Identity ()
y = mapC (+ 1)

z :: ConduitT Integer Void Identity [Integer]
z = sinkList

run :: ConduitT () Void Identity r -> r
run = runConduitPure

ex4 = run $ x .| y .| z

-----------------------------------------------------------

mapC' :: Monad m => (a -> b) -> ConduitT a b m ()
mapC' = mapC

foldlC' :: Monad m => (a -> b -> a) -> a -> ConduitT b o m a
foldlC' = foldlC

mapM_C' :: Monad m => (a -> m ()) -> ConduitT a o m ()
mapM_C' = mapM_C

repeatC' :: Monad m => a -> ConduitT i a m ()
repeatC' = repeatC

takeWhileC' :: Monad m => (a -> Bool) -> ConduitT a a m ()
takeWhileC' = takeWhileC

decodeUtf8C' :: MonadThrow m => ConduitT ByteString Text m ()
decodeUtf8C' = decodeUtf8C

----------------------------------

loudYield a = do
  liftIO $ print "yield"
  yield a

loudSink = mapM_C (print . (++ "sink") . show)

loudSink2 = do
  liftIO $ print "sink"
  await >>= maybe (liftIO $ print "done") (\i -> (liftIO $ print ("received " ++ show i)) >> loudSink2)

ex5 = runConduit $ mapM_ loudYield [1 .. 3] .| loudSink

ex6 = runConduit $ mapM_ loudYield [1 .. 3] .| loudSink2

-- >>> ex6
-- "sink"
-- "yield"
-- "received 1"
-- "sink"
-- "yield"
-- "received 2"
-- "sink"
-- "yield"
-- "received 3"
-- "sink"
-- "done"
--

--------------------------------------

source = do
  liftIO $ print "source 1"
  yield 1
  liftIO $ print "source 2" -- never printed

sink = do
  liftIO $ print "sink 1"
  await
  liftIO $ print "sink 2"

-- >>> runConduit $ source .| sink
-- "sink 1"
-- "source 1"
-- "sink 2"
--

---------------------------------
-- behaviour of undefined
-- >>> runConduit $ undefined .| return 1
-- >>> runConduit $ return () .| undefined
-- 1

-- *** Exception: Prelude.undefined

-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:75:14 in base:GHC.Err
--   undefined, called at <interactive>:16566:28 in interactive:Ghci3
--

---------------------------------
-- finalizers
s = bracketP (print "acquire") (\_ -> print "release") (\_ -> traverse_ yield [0 .. 10])

ex8 = runConduitRes $ s .| takeC 2 .| printC

ex9 = runConduitRes $ s .| takeC 2 .| (printC >> undefined)

-- >>> ex9
-- "acquire"
-- 0
-- 1
-- "release"

-- *** Exception: Prelude.undefined

-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:75:14 in base:GHC.Err
--   undefined, called at /media/boran/hori/github/haskellnotes/src/Streaming/Conduit1.hs:138:50 in main:Streaming.Conduit1
--

-------------------------------------------

ex10 = runConduitPure $ yieldMany [1 .. 10] .| getZipSink ((/) <$> ZipSink sumC <*> ZipSink (lengthC))

-- >>> ex10
-- 5.5
--
