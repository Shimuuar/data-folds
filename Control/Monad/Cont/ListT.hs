{-# LANGUAGE RankNTypes #-}
-- | Another implementation of ListT monad
module Control.Monad.Cont.ListT (
    ListT(..)
  , runListT  
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- | Monad transformer which add backtracking to monad.
newtype ListT m a = ListT { unListT :: forall r. (r -> a -> m r) -> r -> m r }

-- | Collect all results from ListT monad.
runListT :: Monad m => ListT m a -> m [a]
runListT list = unListT list (\as a -> return (as ++ [a])) []


instance Monad m => Functor (ListT m) where
  fmap f (ListT list) = ListT $ \cont r0 -> list (\r a -> cont r (f a)) r0

instance Monad m => Applicative (ListT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ListT m) where
  return x   = ListT $ \step x0 -> step x0 x
  list >>= f = ListT $ \step ->
    unListT list (\r a -> unListT (f a) step r)


instance Monad m => Alternative (ListT m) where
  empty = ListT $ \_ r -> return r
  ListT contA <|> ListT contB = ListT $ \step r ->
    contB step =<< contA step r


instance MonadTrans ListT where
  lift m = ListT $ \step x0 -> step x0 =<< m

instance MonadIO m => MonadIO (ListT m) where
  liftIO = lift . liftIO



{-
Monadic laws:
 Left identity:    return a >>= f   ≡   f a
 Right identity:   m >>= return     ≡   m
 Associativity:   (m >>= f) >>= g   ≡   m >>= (\x -> f x >>= g)

----------------------------------------------------------------
Proof for left identity

1. (return a) >>= f
  -- Inline >>=
2. ListT $ \step x0 → unListT (return a) (\r b → unListT (f b) step r) x0
  -- Inline return
3. ListT $ \step x0 → (\step' x0' -> step' x0' a) (\r b → unListT (f b) step r) x0
  -- Substitute
4. ListT $ \step x0 → (\r b → unListT (f b) step r) x0 a
  -- Substitute
5. ListT $ \step x0 → unListT (f a) step x0
  -- Lemma 1
6. f a
  QED


----------------------------------------------------------------
-- Proof for right identity

1. m >>= return
  -- Inline >>=
2. ListT $ \step x0 → unListT m (\r a → unListT (return a) step r) x0
  -- Inline return
3. ListT $ \step x0 → unListT m (\r a → (\step' x0' → step' x0' a) step r) x0
  -- Substitute
4. ListT $ \step x0 → unListT m (\r a → step r a) x0
  -- Eta-reduce
5. ListT $ \step x0 → unListT m step x0
  -- Lemma 1
6. m
  QED


----------------------------------------------------------------
-- Proof for associativity

1. 


----------------------------------------------------------------
-- Lemma 1
--
-- Prove:
--   xs = ListT $ \step x0 → unListT xs step x0

1. ListT $ \step x0 → unListT xs step x0
  -- Eta-reduce
2. ListT (unListT xs)
  -- Cast
3. xs
  QED

-}
