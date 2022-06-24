module Free.FreeTransformer where

newtype FreeT f m a = FreeT {runFreeT :: m (FreeF f a (FreeT f m a))}

data FreeF f a b = Pure a | Free (f b)

--newtype ListT m a = ListT {next :: m (Step m a)}
--
--data Step m a = Cons a (ListT m a) | Nil deriving (Functor)


