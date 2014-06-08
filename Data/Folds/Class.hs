{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Type classes for folds data types
module Data.Folds.Class where

import Control.Category
import Control.Monad

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



----------------------------------------------------------------
-- Folds
----------------------------------------------------------------

-- | Type class for pure manifest folds.
class PureFold fold where
  -- | Extract current value from fold
  extractFold :: fold a b -> b
  -- | Push one element into fold
  feedFold :: a -> fold a b -> fold a b

-- | Type class for monadic manifest folds.
class MonadicFold fold where
  -- | Extract current value from fold
  extractFoldM :: Monad m => fold m a b -> m b
  -- | Push one element into fold
  feedFoldM :: Monad m => a -> fold m a b -> m (fold m a b)

-- | Execute fold with attached data source
runFold :: (PureFold fold) => fold () a -> a
runFold = extractFold . feedFold ()

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
