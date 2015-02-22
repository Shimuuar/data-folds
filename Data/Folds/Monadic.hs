{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
module Data.Folds.Monadic (
    -- * Monadic folds
    FoldM(..)
  , FoldST(..)
    -- * Blueprint fold
  , FoldGen(..)
  ) where

import Control.Applicative
import Control.Monad

import Data.Folds.Class    hiding (pipe)
import Data.Folds.Internal



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
  feedOneM a (FoldM step x out) = do
    x' <- step x a
    return $ FoldM step x' out
  feedManyM as (FoldM step x0 out) = do
    let step' mx b = do { x <- mx; step x b }
    x' <- foldDataSample as step' (return x0)
    return $ FoldM step x' out

instance MonadicFold FoldST where
  extractFoldM (FoldST _ out) = out
  feedOneM a fold@(FoldST inp _) = inp a >> return fold
  feedManyM as fold@(FoldST inp _) = do
    let step' m b = m >> inp b
    foldDataSample as step' (return ())
    return fold

-- FIXME: Here we can run into efficiency problem with wrong order of
--        >>= nesting. feedManyM is affected as well
instance Monad m => FiniCat Pipette (FoldM m) where
  FoldM step x0 out +<< Pipette p
    = FoldM (\x a -> foldDataSample (p a) step' (return x)) x0 out
    where
      step' mx b = do x <- mx
                      step x b

instance Monad m => FiniCat Pipette (FoldST m) where
  FoldST inp out +<< Pipette p
    = FoldST (\a -> foldDataSample (p a) step' (return ()))
             out
    where
      step' m b = m >> inp b



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
  FoldGen mfold +<< pipe = FoldGen $ do
    fold <- mfold
    return $ fold +<< pipe
