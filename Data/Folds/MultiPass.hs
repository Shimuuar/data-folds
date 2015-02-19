{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
-- | Folds that allow multiple pass over data
module Data.Folds.MultiPass (
    MFold(..)
  , mfold
  ) where

import Control.Applicative
import Control.Monad

import Data.Folds.Class
import Data.Folds.Pipette (Pipette(..))
import Data.Folds.Left


----------------------------------------------------------------
-- Multiple pass fold
----------------------------------------------------------------

-- | Fold which may require several passes over same data set.  It
--   tries to minimize number of data traversals, Only monadic bind
--   introduce need to traverse data twice. Applicative interface will
--   traverse data only once like simple 'Fold'
data MFold a b
  = OneStage (Fold a b)
  | forall y. Staged   (Fold a y) (y -> MFold a b)

-- | Lift simple left fold into multiple pass one.
mfold :: Fold a b -> MFold a b
mfold = OneStage

instance Functor (MFold a) where
  fmap f (OneStage fold)    = OneStage (fmap f fold)
  fmap f (Staged fold next) = Staged fold ((fmap . fmap) f next)

instance Applicative (MFold a) where
  pure = OneStage . pure
  OneStage foldA <*> OneStage foldB = OneStage (foldA <*> foldB)
  OneStage foldA <*> Staged foldB next
    = Staged ((,) <$> foldA <*> foldB) (\(f,y) -> f <$> next y)
  Staged foldA next <*> OneStage foldB
    = Staged ((,) <$> foldA <*> foldB) (\(y,a) -> ($ a) <$> next y)
  Staged foldA nextA <*> Staged foldB nextB
    = Staged ((,) <$> foldA <*> foldB) (\(x,y) -> nextA x <*> nextB y)

instance Monad (MFold a) where
  return = pure
  OneStage fold    >>= f = Staged fold f
  Staged fold next >>= f = Staged fold (next >=> f)

instance FiniCat Pipette MFold where
  OneStage fold +<< pipe = OneStage (fold +<< pipe)
  Staged fold next +<< pipe
    = Staged (fold +<< pipe) (\y -> next y +<< pipe)

instance PureFold MFold where
  extractFold (OneStage fold)    = extractFold fold
  extractFold (Staged fold next) = extractFold $ next $ extractFold fold

  feedOne a = feedMany (Pipette $ \step r _ -> step r a)
  feedMany src (OneStage fold)
    = OneStage (feedMany src fold)
  feedMany src (Staged fold next)
    = Staged (feedMany src fold) (feedMany src <$> next)
