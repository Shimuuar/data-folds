{-# LANGUAGE RankNTypes #-}
module Data.Folds.Pipette (
    -- * Stream processors
    Pipette(..)
  , PipetteM(..)
  , Source
  , SourceM
  , toPipetteM
    -- ** Primitives
  , pipe
  , cut
  , flatten
  , flatMap
  ) where

import Control.Arrow        (Arrow(..))
import Control.Category
import Control.Monad
import Data.Monoid
import qualified Data.Foldable as T
import Prelude hiding (id,(.))


----------------------------------------------------------------
-- Stream processors
----------------------------------------------------------------

-- | Simple stream processor. It transforms stream elementwise.
data Pipette a b = Pipette (forall r. (r -> b -> r) -> (r -> a -> r))

-- | Monadic stream processor.
data PipetteM m a b = PipetteM (forall r. (r -> b -> m r) -> (r -> a -> m r))

-- | Pure data source. It's really a CPS encoded list
type Source    a = Pipette    () a

-- | Monadic data source.
type SourceM m a = PipetteM m () a


-- | Convert pure stream processor to monadic
toPipetteM :: Monad m => Pipette a b -> PipetteM m a b
toPipetteM (Pipette pipette) = PipetteM $ \contM r0 ->
  pipette (\mr b -> do{ r <- mr; contM r b }) (return r0)



instance Category Pipette where
  id = Pipette id
  Pipette pipeBC . Pipette pipeAB = Pipette $ \contC ->
    pipeAB (pipeBC contC)

instance Category (PipetteM m) where
  id = PipetteM id
  PipetteM pipeBC . PipetteM pipeAB = PipetteM $ \contC ->
    pipeAB (pipeBC contC)


instance Arrow Pipette where
  arr f = Pipette $ \cont r a -> cont r (f a)
  first (Pipette contAB) = Pipette $ \contAD_BD r (a,d) ->
    contAB (\r' b -> contAD_BD r' (b,d)) r a

instance Monad m => Arrow (PipetteM m) where
  arr f = PipetteM $ \cont r a -> cont r (f a)
  first (PipetteM contAB) = PipetteM $ \contAD_BD r (a,d) ->
    contAB (\r' b -> contAD_BD r' (b,d)) r a


instance Monoid (Pipette a b) where
  mempty = Pipette $ \_ r _ -> r
  Pipette pipeA `mappend` Pipette pipeB = Pipette $ \cont r a ->
    pipeB cont (pipeA cont r a) a

instance Monad m => Monoid (PipetteM m a b) where
  mempty = PipetteM $ \_ r _ -> return r
  PipetteM pipeA `mappend` PipetteM pipeB = PipetteM $ \cont r a -> do
    r' <- pipeA cont r a
    pipeB cont r' a


----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

pipe :: (a -> b) -> Pipette a b
pipe f = Pipette $ \cont r a -> cont r (f a)

-- | Same as /filter/. Apply cut to data.
cut :: (a -> Bool) -> Pipette a a
cut p = Pipette $ \cont r a -> if p a then cont r a else r
{-# INLINE cut #-}

flatten :: T.Foldable f => Pipette (f a) a
flatten = Pipette $ \cont r a -> T.foldl' cont r a
{-# INLINE flatten #-}

flatMap :: T.Foldable f => (a -> f b) -> Pipette a b
flatMap f = flatten <<< arr f
{-# INLINE flatMap #-}
