{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DeriveDataTypeable    #-}
-- | Type classes for folds data types
module Data.Folds.Class (
    -- * Category-like type class
    FiniCat(..)
  , (>>+)
  , InitCat(..)
  , (+>>)
  , (>>>)
  , (<<<)
    -- * Data sample
  , DataSample(..)
  , Sample(..)
    -- * Pipettes
  , Pipette(..)
  , pipe
  , cut
  , flatten
  , flatMap
    -- * Stateful folds API
  , PureFold(..)
  , runFold
  , MonadicFold(..)
  , runFoldM
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad

import Data.Typeable (Typeable)
import Data.Monoid
import Data.List (foldl')
import qualified Data.Foldable as T

import Prelude hiding (id,(.))



----------------------------------------------------------------
-- Category-like composition
----------------------------------------------------------------

-- | Category-like type class where object of @fini@ type can appear
--   only at end side of composition. It must obey law similar to ones
--   of category:
--
--   > f +<< id = f                        - identity
--   > f +<< (g <<< h) = (f +<< g) +<< h   - associativity
class Category cat => FiniCat cat fini where
  (+<<) :: fini b c -> cat a b -> fini a c
infixr 1 +<<

-- | Synonym for 'composeFini' with arguments flipped
(>>+) :: (FiniCat cat fini) => cat a b -> fini b c -> fini a c
(>>+) = flip (+<<)
infixr 1 >>+


-- | Same as 'FiniCat' but composition is from other side
class Category cat => InitCat ic cat where
  (<<+) :: cat a b -> ic a -> ic b
infixr 1 <<+

-- | Synonym for 'composeFini' with arguments flipped
(+>>) :: (InitCat ic cat) => ic a -> cat a b -> ic b
(+>>) = flip (<<+)
infixl 1 +>>



----------------------------------------------------------------
-- Data sample
----------------------------------------------------------------

-- | Data sample which could be consumed with left fold
newtype DataSample a = DataSample
  { foldDataSample :: forall r. (r -> a -> r) -> r -> r }
  deriving (Typeable)

instance Functor DataSample where
  fmap f (DataSample list) = DataSample $ \step x0 ->
    list (\r a -> step r (f a)) x0

instance Applicative DataSample where
  pure a = DataSample $ \step r0 -> step r0 a
  (<*>)  = ap

instance Monad DataSample where
  return a = DataSample $ \step r0 -> step r0 a
  DataSample list >>= f = DataSample $ \step x0 ->
    list (\r a -> foldDataSample (f a) step r) x0

instance Alternative DataSample where
  empty = DataSample $ \_ r -> r
  DataSample contA <|> DataSample contB = DataSample $ \step x0 ->
    contB step (contA step x0)

instance Monoid (DataSample a) where
  mempty  = empty
  mappend = (<|>)


-- | Type class for data samples
class Sample v where
  type Elem v :: *
  asDataSample :: v -> DataSample (Elem v)

instance Sample [a] where
  type Elem [a] = a
  asDataSample xs = DataSample $ \step x -> foldl' step x xs



----------------------------------------------------------------
-- Data transformations
----------------------------------------------------------------

-- | Data transformer
--
-- FIXME: describe instance meaning
newtype Pipette a b = Pipette (a -> DataSample b)


-- | Lift function to pipette. Same as 'arr'
pipe :: (a -> b) -> Pipette a b
pipe = arr

-- | Filter input of function
cut :: (a -> Bool) -> Pipette a a
cut f = Pipette $ \a -> if f a then pure a else empty

-- | Feed all elements of `f a` into downstream fold
flatten :: T.Foldable f => Pipette (f a) a
flatten = Pipette $ \fa -> DataSample $ \step r -> T.foldl' step r fa
{-# INLINE flatten #-}

-- | Feed elements of result of functions to downstream fold
flatMap :: T.Foldable f => (a -> f b) -> Pipette a b
flatMap f = flatten <<< arr f
{-# INLINE flatMap #-}

instance Category Pipette where
  id = Pipette pure
  -- FIXME: this is just a Kleisli category. Is >=> efficient?
  Pipette f . Pipette g = Pipette $ \a -> DataSample $ \step r0 ->
    foldDataSample (g a) (\r b -> foldDataSample (f b) step r)  r0

instance Arrow Pipette where
  arr f = Pipette $ \a -> DataSample (\step r -> step r (f a))
  first  (Pipette f) = Pipette $ \ ~(b,d) -> f b >>= \c -> return (c,d)
  second (Pipette f) = Pipette $ \ ~(d,b) -> f b >>= \c -> return (d,c)

instance Functor (Pipette a) where
  fmap f (Pipette p) = Pipette $ \a -> fmap f (p a)

instance Applicative (Pipette a) where
  pure = Pipette . pure . pure
  Pipette f <*> Pipette g = Pipette $ liftA2 (<*>) f g

instance Monad (Pipette a) where
  return = pure
  Pipette m >>= f = Pipette $ \a -> do
    b <- m a
    case f b of
      Pipette c -> c a

instance Monoid (Pipette a b) where
  mempty = Pipette $ const empty
  Pipette f `mappend` Pipette g = Pipette $ \a ->
    f a <> g a

instance InitCat DataSample Pipette where
  Pipette p <<+ list = list >>= p



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
  feedMany :: DataSample a -> fold a b -> fold a b


-- | Type class for monadic manifest folds.
class MonadicFold fold where
  -- | Extract current value from fold
  extractFoldM :: Monad m => fold m a b -> m b
  -- | Push one element into fold
  feedOneM :: Monad m => a -> fold m a b -> m (fold m a b)
  -- | Push full data sample
  feedManyM :: Monad m => DataSample a -> fold m a b -> m (fold m a b)

-- | Execute fold with attached data source
runFold :: (PureFold fold, Sample v, Elem v ~ a)
        => fold a b -> v -> b
runFold fold xs
  = extractFold $ feedMany (asDataSample xs) fold

-- | Execute monadic fold with attached data source
runFoldM :: (Monad m, MonadicFold fold, Sample v, Elem v ~ a)
         => fold m a b -> v -> m b
runFoldM fold xs
  = extractFoldM =<< feedManyM (asDataSample xs) fold
