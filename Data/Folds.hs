{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | Concrete folds for
module Data.Folds (
    -- * Pure fold
    Fold(..)
    -- * Monadic fold
  , FoldM(..)
  , FoldST(..)
    -- * Monadic blueprint fold
  , FoldGen(..)
  ) where

import Control.Applicative
import Control.Monad

import Data.Folds.Class
import Data.Folds.Pipette


----------------------------------------------------------------
-- Strict left fold
----------------------------------------------------------------

-- | Strict left pure fold.
data Fold a b = forall x. Fold (x -> a -> x) !x (x -> b)

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
  feedFold a (Fold step x out) = Fold step (step x a) out



----------------------------------------------------------------
-- Monadic folds
----------------------------------------------------------------

-- | Monadic strict left fold
data FoldM m a b = forall x. FoldM (x -> a -> m x) !x (x -> m b)

-- | Another variant of monadic fold where fold state is hidden in closure
data FoldST m a b = FoldST (a -> m ()) (m b)


instance Monad m => Functor (FoldM m a) where
  fmap f (FoldM step x done) = FoldM step x (liftM f . done)

instance Monad m => Functor (FoldST m a) where
  fmap f (FoldST inp done) = FoldST inp (liftM f done)


instance Monad m => Applicative (FoldM m a) where
  pure x = FoldM (\s _ -> return s) () (\_ -> return x)
  FoldM stepA xA0 doneA <*> FoldM stepB xB0 doneB
    = FoldM (\(Pair xA xB) a -> do xA' <- stepA xA a
                                   xB' <- stepB xB a
                                   return $! Pair xA' xB'
            )
            (Pair xA0 xB0)
            (\(Pair xA xB) -> do a <- doneA xA
                                 b <- doneB xB
                                 return $ a b
            )

instance Monad m => Applicative (FoldST m a) where
  pure x = FoldST (\_ -> return ()) (return x)
  FoldST inpA outA <*> FoldST inpB outB
    = FoldST (\a -> inpA a >> inpB a) (liftM2 ($) outA outB)

instance MonadicFold FoldM where
  extractFoldM (FoldM _ x out) = out x
  feedFoldM a (FoldM step x out) = do x' <- step x a
                                      return $ FoldM step x' out

instance MonadicFold FoldST where
  extractFoldM (FoldST _ out) = out
  feedFoldM a fold@(FoldST inp _) = inp a >> return fold

instance Monad m => FiniCat Pipette (FoldM m) where
  composeFini fold pipe
    = composeFini fold (toPipetteM pipe)
instance (m ~ m', Monad m) => FiniCat (PipetteM m) (FoldM m') where
  composeFini (FoldM step x0 done) (PipetteM pipe)
    = FoldM (pipe step) x0 done

instance Monad m => FiniCat Pipette (FoldST m) where
  composeFini fold pipe
    = composeFini fold (toPipetteM pipe)
instance (m ~ m', Monad m) => FiniCat (PipetteM m') (FoldST m) where
  composeFini (FoldST inp out) (PipetteM pipe)
    = FoldST (pipe (const inp) ()) out



----------------------------------------------------------------
-- Blueprint fold
----------------------------------------------------------------

-- | Blueprint fold. It doesn't perform any work by itself but contain
--   closure with description how to create fold which does all the
--   work.
newtype FoldGen fold m a b = FoldGen { runFoldGen :: m (fold m a b) }

instance (Monad m, Functor (fold m a)) => Functor (FoldGen fold m a) where
  fmap f (FoldGen make) = FoldGen $ liftM (fmap f) make

instance (Monad m, Applicative (fold m a)) => Applicative (FoldGen fold m a) where
  pure = FoldGen . return . pure
  FoldGen makeA <*> FoldGen makeB = FoldGen $ liftM2 (<*>) makeA makeB

instance (Monad m, FiniCat pipe (fold m)) => FiniCat pipe (FoldGen fold m) where
  composeFini (FoldGen mfold) pipe = FoldGen $ do
    fold <- mfold
    return $ composeFini fold pipe




----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

data Pair a b = Pair !a !b
