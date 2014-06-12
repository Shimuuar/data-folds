{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
-- | Folds that allow multiple pass over data
module Data.Folds.MultiPass (
    MFold(..)
  , mfold
  ) where

import Control.Arrow ((>>>))
import Control.Applicative

import Data.Folds.Class
import Data.Folds.Pipette
import Data.Folds.Left


----------------------------------------------------------------
-- Multiple pass fold
----------------------------------------------------------------

-- | Folds which may require several passes over same data set.
-- data MFold a b = MFold { runMFold :: Source a -> b }


data MFold a b = forall y. MFold (Source a -> y) (y -> Fold a b)


mfold :: Fold a b -> MFold a b
mfold fold = MFold (const ()) (const fold)

instance Functor (MFold a) where
  fmap f (MFold prev fold) = MFold prev ((fmap . fmap) f fold)

instance Applicative (MFold a) where
  pure = mfold . pure
  MFold prevA foldA <*> MFold prevB foldB = MFold
    (\src -> (prevA src, prevB src))
    (\(a,b) -> foldA a <*> foldB b)

instance Monad (MFold a) where
  return = pure
  MFold prev fold >>= f
    = MFold prev' id
    where
      prev' src = let y = prev src
                      a = runFold (fold y +<< src)
                  in case f a of
                       MFold pr fld -> fld (pr src)

instance FiniCat Pipette MFold where
  composeFini (MFold prev fold) pipe
    = MFold (\src -> prev (src >>> pipe))
            (\y   -> fold y +<< pipe)
