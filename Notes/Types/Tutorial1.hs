{-# LANGUAGE AllowAmbiguousTypes #-}
-- https://www.youtube.com/watch?v=6FRJfEhlqyg&ab_channel=BerlinFunctionalProgrammingGroup
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Types.Tutorial1 where

import Data.Kind
import GHC.TypeLits
import Language.Haskell.TH.Lens (_Overlappable)

type Debug = 'True

data User = User {userName :: Username, password :: Password}

newtype Username = Username String

newtype Password = Password String

deriving instance Show User

deriving instance Show Username

-- deriving instance (DebugOn Debug) => Show Password

class DebugOn (d :: Bool)

instance DebugOn 'True

instance TypeError (Text "Cannot print in production mode") => DebugOn 'False

-- instance {-# INCOHERENT #-} (Debug ~ 'True, a ~ Password) => Show a where
--  show _ = ""
--
-- instance {-# INCOHERENT #-} (Debug ~ 'False, a ~ Password) => Show a where
--  show _ = ""

testUser = User (Username "a") (Password "b")

-- >>> testUser
-- User {userName = Username "a", password = Password "b"}
--

-------------------------

instance {-# OVERLAPPABLE #-} Show a where
  show _ = "unprintable"

-- >>> show not
-- "unprintable"
--

instance {-# INCOHERENT #-} a ~ [] => Foldable a where
  foldr = foldr

-- >>> :t foldr
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--

-------------------

class Nth (n :: Nat) (tuple :: Type) (out :: Type) | n tuple -> out where
  nth :: tuple -> out

instance Nth 0 (x, y) x where
  nth (a, b) = a

instance {-# OVERLAPPABLE #-} (Nth (n - 1) y out) => Nth n (x, y) out where
  nth (a, b) = nth @(n - 1) b

a = nth @2 (1, (1, 2))

-- >>> a
-- 1
--
