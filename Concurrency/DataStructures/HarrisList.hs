module DataStructures.HarrisList where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Aeson.Encoding (list)
import Data.Atomics
import Data.Foldable (traverse_)
import Data.IORef

data TaggedPtr a = TaggedPtr {deleted :: {-# UNPACK #-} !(Bool), item :: {-# UNPACK #-} !(a)}

newtype List k v = List (IORef (TaggedPtr (Node k v)))

data Node k v = Node {key :: !k, value :: !v, next :: (IORef (TaggedPtr (Node k v)))} | E

data Cursor k v = Cursor
  { prev :: IORef (TaggedPtr (Node k v)),
    cur :: Ticket (TaggedPtr (Node k v))
  }

getElem (Cursor _ v) = value $ item $ peekTicket v

newNode k v = (Node k v) <$> newIORef (TaggedPtr False E)

new = List <$> newIORef (TaggedPtr False E)

find_harris_michael :: (Ord k) => List k v -> k -> IO (Bool, Cursor k v)
find_harris_michael l@(List ioRef) key = makeCursor >>= go
  where
    makeCursor = Cursor ioRef <$> readForCAS ioRef
    go c@(Cursor prev cur) = do
      case item $ peekTicket cur of
        E -> return $ (False, c)
        (Node key' _ next) -> do
          nextTicket <- readForCAS next
          case peekTicket nextTicket of
            (TaggedPtr True addr) -> do
              (b, newTicket) <- casIORef prev cur (TaggedPtr False addr) -- deleted
              if b then go (Cursor prev newTicket) else find_harris_michael l key
            (TaggedPtr False addr) ->
              case compare key key' of
                LT -> return $ (False, c)
                EQ -> return $ (True, c)
                GT -> go (Cursor next nextTicket)

insertC :: Cursor k v -> Node k v -> IO (Bool, Ticket (TaggedPtr (Node k v)))
insertC c@(Cursor prev cur) node@(Node k v next) = do
  writeIORef next (peekTicket cur)
  casIORef prev cur (TaggedPtr False node)

deleteC :: Cursor k a -> IO (Maybe a)
deleteC c@(Cursor prev cur) = do
  let TaggedPtr _ c = peekTicket cur
  case c of
    E -> return Nothing
    (Node _ v next) -> do
      nextTicket <- readForCAS next
      case peekTicket nextTicket of
        (TaggedPtr True addr) -> return Nothing
        (TaggedPtr False addr) -> do
          (b, _) <- casIORef next nextTicket (TaggedPtr True addr)
          if b
            then do
              _ <- casIORef prev cur (TaggedPtr False addr)
              return (Just v)
            else return Nothing

lookup find list k = do
  (found, cursor) <- find list k
  if found
    then return Nothing
    else getElem cursor

delete find list k = do
  (found, cursor) <- find list k
  if not found
    then return Nothing
    else do
      v <- deleteC cursor
      case v of
        Nothing -> delete find list k
        x -> return x

insert find list k v = newNode k v >>= go
  where
    go node = do
      (found, cursor) <- find list k
      if found
        then return False
        else do
          (ok, _) <- insertC cursor node
          if ok then return True else go node

allItems (List h) = readIORef h >>= (go . item)
  where
    go n = do
      case n of
        E -> return []
        (Node a b next) -> do
          TaggedPtr deleted n <- readIORef next
          if deleted
            then go n
            else ((a, b) :) <$> go n

insert' :: (Ord k) => List k v -> k -> v -> IO (Bool)
insert' = insert find_harris_michael

delete' :: (Ord k) => List k v -> k -> IO (Maybe v)
delete' = delete find_harris_michael

test1 = do
  list <- new
  a <- insert' list 1 11
  b <- insert' list 2 12
  c <- insert' list 2 12
  x <- delete' list 1
  y <- delete' list 2
  z <- delete' list 2
  return (a, b, c, x, y, z)

-- >>> test1
-- (True,True,False,Just 11,Just 12,Nothing)
--

test11 = do
  list <- new
  a <- insert' list 1 11
  y <- allItems list

  x <- delete' list 1
  y' <- allItems list
  x' <- delete' list 1
  y'' <- allItems list

  print (a, x, y, y', x', y'')

-- >>> test11
-- (True,Just 11,[(False,1,11)],[],Nothing,[])
--

newtype LockedList a = LockedList (MVar [a])

newL = LockedList <$> (newMVar [])

insertL (LockedList list) k v = modifyMVar_ list (return . go)
  where
    go [] = [(k, v)]
    go xss@((k', v') : xs)
      | k' == k = xss
      | k < k' = (k, v) : xss
      | otherwise = (k', v') : (go xs)

deleteL (LockedList list) k = modifyMVar_ list (return . go)
  where
    go [] = []
    go xss@((k', v') : xs)
      | k' == k = xs
      | k < k' = xss
      | otherwise = (k', v') : (go xs)

allItemsL :: LockedList a -> IO [a]
allItemsL (LockedList list) = readMVar list

pushN nthreads nitems f arr = mapConcurrently_ (\x -> traverse_ (\a -> f arr a a) [x * nitems .. (((x + 1) * nitems) - 1)]) [0 .. (nthreads - 1)]

deleteN nthreads nitems f arr = mapConcurrently_ (\x -> traverse_ (\a -> f arr a) [x * nitems .. (((x + 1) * nitems) - 1)]) [0 .. (nthreads - 1)]

test2 = do
  list <- new
  traverse (\x -> insert' list (x + 1) x) [0 .. 10]
  x <- allItems list
  f <- traverse (\x -> delete' list (x + 1)) [0 .. 10]
  y <- allItems list

  print x
  print y
  print f

test3 nthreads nitems = do
  list <- new
  pushN nthreads nitems insert' list
  y <- allItems list
  print $ sum $ fmap fst y

test3' nthreads nitems = do
  list <- newL
  pushN nthreads nitems insertL list
  y <- allItemsL list
  print $ sum $ fmap fst y

-- >>> test3'
-- 499500
--
test4 nthreads nitems = do
  list <- new
  pushN nthreads nitems insert' list
  y <- allItems list
  print $ sum $ fmap fst y
  deleteN nthreads nitems delete' list
  y <- allItems list
  print $ sum $ fmap fst y
