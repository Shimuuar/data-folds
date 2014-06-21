{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
-- | Type classes for folds data types
module Data.Folds.Class (
    -- * Category-like type class
    FiniCat(..)
  , (+<<)
  , (>>+)
    -- * Monoid accumulators
  , Accumulator(..)
  , Count(..)
    -- * Stateful folds API
  , PureFold(..)
  , runFold
  , MonadicFold(..)
  , runFoldM
    -- * Data samples
  , Sample(..)
  ) where

import Control.Category
import Control.Monad

import Data.Monoid
import Data.List (foldl')
import Data.Folds.Pipette

import Prelude hiding (id,(.))



----------------------------------------------------------------
-- Composition
----------------------------------------------------------------

-- | Category-like type class where object of @fini@ type can appear
--   only at end side of composition. It must obey law similar to ones
--   of category:
--
--   > f +<< id = f                        - identity
--   > f +<< (g <<< h) = (f +<< g) +<< h   - associativity
class Category cat => FiniCat cat fini where
  composeFini :: fini b c -> cat a b -> fini a c

-- | Synonym for 'composeFini'
(+<<) :: (FiniCat cat fini) => fini b c -> cat a b -> fini a c
(+<<) = composeFini
infixr 1 +<<

-- | Synonym for 'composeFini' with arguments flipped
(>>+) :: (FiniCat cat fini) => cat a b -> fini b c -> fini a c
(>>+) = flip composeFini
infixr 1 >>+


-- | Type class for monoidal accumulators
class Monoid m => Accumulator m a where
  -- | Convert value to 1-element accumulator
  unit :: a -> m
  unit a = cons a mempty
  -- | Prepend value to accumulator
  cons :: a -> m -> m
  cons a m = unit a <> m
  -- | Append value to accumulator
  snoc :: m -> a -> m
  snoc m a = m <> unit a

newtype Count a = Count { getCount :: Int }

instance Monoid (Count a) where
  mempty = Count 0
  mappend (Count a) (Count b) = Count (a + b)

instance Accumulator (Count a) a where
  unit _ = Count 1


instance Accumulator () a where
  unit _ = ()

instance Num a => Accumulator (Sum a) a where
  unit = Sum

instance Num a => Accumulator (Product a) a where
  unit = Product

instance Accumulator Any Bool where
  unit = Any

instance Accumulator All Bool where
  unit = All

instance Accumulator (Endo a) (a -> a) where
  unit = Endo



----------------------------------------------------------------
-- Folds
----------------------------------------------------------------

-- | Type class for pure manifest folds.
class PureFold fold where
  -- | Extract current value from fold
  extractFold :: fold a b -> b
  -- | Push one element into fold  
  feedOne  :: a -> fold a b -> fold a b
  -- | Feed sample to the fold
  feedMany :: Source a -> fold a b -> fold a b


-- | Type class for monadic manifest folds.
class MonadicFold fold where
  -- | Extract current value from fold
  extractFoldM :: Monad m => fold m a b -> m b
  -- | Push one element into fold
  feedFoldM :: Monad m => a -> fold m a b -> m (fold m a b)

-- | Execute fold with attached data source
runFold :: (PureFold fold) => fold () a -> a
runFold = extractFold . feedOne ()

-- | Execute monadic fold with attached data source
runFoldM :: (Monad m, MonadicFold fold) => fold m () a -> m a
runFoldM = extractFoldM <=< feedFoldM ()



----------------------------------------------------------------
-- Data sample
----------------------------------------------------------------

-- | Data sample. This type class is variation of
--   'Data.Foldable.Foldable' but elements' type is represented by
--   type family to allow monomorphic containers like text and
--   bytestring. Every instance of this type class should describe how
--   to fold itself.
class Sample s where
  type Element s :: *
  toSource :: s -> Source (Element s)

instance Sample [a] where
  type Element [a] = a
  toSource xs = Pipette $ \step x _ -> foldl' step x xs
