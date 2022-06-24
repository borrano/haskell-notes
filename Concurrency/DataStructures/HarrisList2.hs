{-# LANGUAGE RecordWildCards #-}

module DataStructures.HarrisList2 where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Aeson.Encoding (list)
import Data.Atomics
import Data.Foldable (traverse_)
import Data.IORef

newtype List k = List (IORef ((Node k)))

data Node k
  = Node {key :: !k, next :: (IORef ((Node k)))}
  | Deleted {nextNode :: (Node k)}
  | E

data Cursor k = Cursor
  { prev :: IORef ((Node k)),
    cur :: Ticket ((Node k))
  }

newNode k = (Node k) <$> newIORef (E)

new = List <$> newIORef E

find_harris_michael :: (Ord k) => List k -> k -> IO (Bool, Cursor k)
find_harris_michael l@(List ioRef) key = makeCursor >>= go
  where
    makeCursor = Cursor ioRef <$> readForCAS ioRef
    go c@(Cursor prev cur) = do
      case peekTicket cur of
        E -> return $ (False, c)
        (Node key' next) -> do
          nextTicket <- readForCAS next
          case peekTicket nextTicket of
            (Deleted next) -> do
              (b, newTicket) <- casIORef prev cur next -- deleted
              if b then go (Cursor prev newTicket) else find_harris_michael l key
            _ ->
              case compare key key' of
                LT -> return $ (False, c)
                EQ -> return $ (True, c)
                GT -> go (Cursor next nextTicket)
        _ -> undefined

insertC :: Cursor k -> Node k -> IO (Bool, Ticket ((Node k)))
insertC c@(Cursor prev cur) node@(Node k next) = do
  writeIORef next (peekTicket cur)
  casIORef prev cur (node)

lookup find list k = do
  (found, cursor) <- find list k
  if found
    then return Nothing
    else return $ Just $ key $ peekTicket $ cur cursor

deleteC :: Cursor a -> IO (Maybe a)
deleteC c@(Cursor prev cur) = do
  case peekTicket cur of
    E -> return Nothing
    (Node x next) -> do
      nextTicket <- readForCAS next
      case peekTicket nextTicket of
        (Deleted {}) -> return Nothing
        n -> do
          (b, _) <- casIORef next nextTicket (Deleted n)
          if b
            then do
              _ <- casIORef prev cur n
              return (Just x)
            else return Nothing

delete find list k = do
  (found, cursor) <- find list k
  if not found
    then return Nothing
    else do
      v <- deleteC cursor
      case v of
        Nothing -> delete find list k
        x -> return x

insert find list k = newNode k >>= go
  where
    go node = do
      (found, cursor) <- find list k
      if found
        then return False
        else do
          (ok, _) <- insertC cursor node
          if ok then return True else go node

allItems (List h) = readIORef h >>= (go)
  where
    go n = do
      case n of
        E -> return []
        (Node a next) -> do
          x <- readIORef next
          case x of
            Deleted {nextNode} -> go nextNode
            _ -> (a :) <$> go x

insert' :: (Ord k) => List k -> k -> IO (Bool)
insert' = insert find_harris_michael

delete' :: (Ord k) => List k -> k -> IO (Maybe k)
delete' = delete find_harris_michael

test1' = do
  list <- new
  a <- insert' list 1
  b <- insert' list 2
  c <- insert' list 2
  x <- delete' list 1
  y <- delete' list 2
  z <- delete' list 2
  return (a, b, c, x, y, z)

---- >>> test1'
---- (True,True,False,Just 1,Just 2,Nothing)
----
--
test11' = do
  list <- new
  a <- insert' list 1
  y <- allItems list

  x <- delete' list 1
  y' <- allItems list
  x' <- delete' list 1
  y'' <- allItems list
  print (a, x, y, y', x', y'')

---- >>> test11'
---- (True,Just 1,[1],[],Nothing,[])
----

pushN nthreads nitems f arr = mapConcurrently_ (\x -> traverse_ (\a -> f arr a) [x * nitems .. (((x + 1) * nitems) - 1)]) [0 .. (nthreads - 1)]

deleteN nthreads nitems f arr = mapConcurrently_ (\x -> traverse_ (\a -> f arr a) [x * nitems .. (((x + 1) * nitems) - 1)]) [0 .. (nthreads - 1)]

test2' = do
  list <- new
  traverse (insert' list) [0 .. 10]
  allItems list >>= print
  traverse (delete' list) [0 .. 12] >>= print
  allItems list >>= print

-- >>> test2'
-- [0,1,2,3,4,5,6,7,8,9,10]
-- [Just 0,Just 1,Just 2,Just 3,Just 4,Just 5,Just 6,Just 7,Just 8,Just 9,Just 10,Nothing,Nothing]
-- []
--

test3' nthreads nitems = do
  list <- new
  pushN nthreads nitems insert' list
  y <- allItems list
  print $ sum y

---- >>> test3' 2 10
---- 190
----
test4' nthreads nitems = do
  list <- new
  pushN nthreads nitems insert' list
  y <- allItems list
  print $ sum $ y
  deleteN nthreads nitems delete' list
  y <- allItems list
  print $ sum $ y
