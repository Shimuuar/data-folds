{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Data.Folds.Accumulator (
    -- * Monoid accumulators
    Accumulator(..)
    -- ** Data types
  , Count(..)
  , Max(..)
  , Min(..)
  ) where

import Data.Monoid

----------------------------------------------------------------
-- Monoidal accumulator
----------------------------------------------------------------

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


newtype Max a = Max { getMax :: Maybe a }

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  mappend (Max Nothing) m = m
  mappend m (Max Nothing) = m
  mappend (Max (Just a)) (Max (Just b)) = Max (Just $! max a b)

instance Ord a => Accumulator (Max a) a where
  snoc (Max (Just a)) b = Max $ Just $! max a b
  snoc (Max Nothing)  b = Max $ Just b
  cons = flip snoc
  unit = Max . Just


newtype Min a = Min { getMin :: Maybe a }


instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  mappend (Min Nothing) m = m
  mappend m (Min Nothing) = m
  mappend (Min (Just a)) (Min (Just b)) = Min (Just $! min a b)

instance Ord a => Accumulator (Min a) a where
  snoc (Min (Just a)) b = Min $ Just $! min a b
  snoc (Min Nothing)  b = Min $ Just b
  cons = flip snoc
  unit = Min . Just



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


-- -- | Convert monoidal accumulator to left fold
-- fromAcc :: Accumulator m a => Fold a m
-- fromAcc = Fold snoc mempty id
