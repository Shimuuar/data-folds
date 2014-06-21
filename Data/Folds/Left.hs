{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
module Data.Folds.Left (
    Fold(..)
  , fromAcc
  ) where

import Control.Applicative
import Data.Monoid

import Data.Folds.Class
import Data.Folds.Pipette
import Data.Folds.Internal



-- | Strict left pure fold.
data Fold a b = forall x. Fold (x -> a -> x) !x (x -> b)

-- | Convert monoidal accumulator to left fold
fromAcc :: Accumulator m a => Fold a m
fromAcc = Fold snoc mempty id

instance Functor (Fold a) where
  fmap f (Fold step x done) = Fold step x (f . done)

instance Applicative (Fold a) where
  pure x = Fold (\s _ -> s) () (\_ -> x)
  Fold stepA xA0 doneA <*> Fold stepB xB0 doneB
    = Fold (\(Pair xA xB) a -> Pair (stepA xA a) (stepB xB a))
           (Pair xA0 xB0)
           (\(Pair xA xB) -> doneA xA (doneB xB))

instance FiniCat Pipette Fold where
  composeFini (Fold step x0 done) (Pipette pipe)
    = Fold (pipe step) x0 done

instance PureFold Fold where
  extractFold (Fold _ x out) = out x
  feedOne a (Fold step x out)
    = Fold step (step x a) out
  feedMany (Pipette cont) (Fold step x0 out)
    = Fold step (cont step x0 ()) out

