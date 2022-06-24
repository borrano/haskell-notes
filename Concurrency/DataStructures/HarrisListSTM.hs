module DataStructures.HarrisListSTM where

import Control.Concurrent.STM
import Control.Monad

newtype List a = List (TVar (Node a))

data Node a
  = Node {val :: a, next :: TVar (Node a)}
  | DNode {next :: TVar (Node a)}
  | E

new = List <$> newTVar E

find :: (Ord a) => List a -> a -> IO (Bool, TVar (Node a))
find (List tvar) key = go tvar
  where
    go prevPtr = join $
      atomically $ do
        prevNode <- readTVar prevPtr
        let curPtr = next prevNode
        curNode <- readTVar curPtr
        case curNode of
          Node {val = y, next = nextNode}
            | (key == y) -> do
                writeTVar curPtr (DNode {next = next curNode})
                return $ return (True, prevPtr)
            | (key > y) -> return (go curPtr)
            | otherwise -> return $ return (False, prevPtr)
          E -> return $ return (False, prevPtr)
          DNode {next = nextNode} ->
            case prevNode of
              Node {} -> writeTVar prevPtr (Node {val = val prevNode, next = nextNode}) >> return (go prevPtr)
              DNode {} -> return (go curPtr)
              _ -> undefined

