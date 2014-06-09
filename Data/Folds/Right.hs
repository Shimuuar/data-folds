{-# LANGUAGE ExistentialQuantification #-}
-- | Right folds
module Data.Folds.Right (
  ) where

import Control.Applicative

import Data.Folds.Class
import Data.Folds.Pipette
import Data.Folds.Internal


----------------------------------------------------------------
-- Right fold
----------------------------------------------------------------

-- | Right fold
data FoldR a b = forall x. FoldR (a -> x -> x) x (x -> b)

instance Functor (FoldR a) where
  fmap f (FoldR step x done) = FoldR step x (f . done)

instance Applicative (FoldR a) where
  pure x = FoldR (\_ s -> s) () (\_ -> x)
  FoldR stepA xA0 doneA <*> FoldR stepB xB0 doneB
    = FoldR (\a ~(Pair xA xB) -> Pair (stepA a xA) (stepB a xB))
            (Pair xA0 xB0)
            (\(Pair xA xB) -> doneA xA $ doneB xB)

instance PureFold FoldR where
  extractFold (FoldR _ x out) = out x
  feedFold a (FoldR step x out) = FoldR step (step a x) out
