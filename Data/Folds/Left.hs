{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
module Data.Folds.Left (
    Fold(..)
  ) where

import Control.Applicative

import Data.Folds.Class
import Data.Folds.Internal



-- | Strict left pure fold.
data Fold a b = forall x. Fold (x -> a -> x) !x (x -> b)

instance Functor (Fold a) where
  fmap f (Fold step x done) = Fold step x (f . done)

instance Applicative (Fold a) where
  pure x = Fold const () (\_ -> x)
  Fold stepA xA0 doneA <*> Fold stepB xB0 doneB
    = Fold (\(Pair xA xB) a -> Pair (stepA xA a) (stepB xB a))
           (Pair xA0 xB0)
           (\(Pair xA xB) -> doneA xA (doneB xB))

instance FiniCat Pipette Fold where
  Fold step x0 done +<< Pipette p
    = Fold (\x a -> foldDataSample (p a) step x) x0 done

instance PureFold Fold where
  extractFold (Fold _ x out) = out x
  feedOne a (Fold step x out)
    = Fold step (step x a) out
  feedMany (DataSample cont) (Fold step x0 out)
    = Fold step (cont step x0) out

