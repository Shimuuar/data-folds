{-# LANGUAGE RankNTypes #-}
module ListT (
    -- * Pure CPS-list
    ListL(..)
  , ListR(..)
  , cpsR
  , cpsL
  , uncpsR
  , uncpsL
    -- * Monad transformer
  , ListT(..)
  , runListT
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class


----------------------------------------------------------------
-- CPS lists
----------------------------------------------------------------

-- | CPS'd list implemented as right-fold
newtype ListR a = ListR (forall r. (a -> r -> r) -> r -> r)

-- | CPS'd list implemented as left-fold
newtype ListL a = ListL (forall r. (r -> a -> r) -> r -> r)


-- | Convert list to CPS form using right fold
cpsR :: [a] -> ListR a
cpsR xs = ListR $ \cons nil -> foldr cons nil xs

-- | Convert list to CPS form using right fold
cpsL :: [a] -> ListL a
cpsL xs = ListL $ \step x0 -> foldl step x0 xs


uncpsR :: ListR a -> [a]
uncpsR (ListR list) = list (:) []

uncpsL :: ListL a -> [a]
uncpsL (ListL list) = list (\xs x -> xs ++ [x]) [] -- FIXME: diff lists!



instance Functor ListR where
  fmap f (ListR list) = ListR $ \cont r0 ->
    list (\a r -> cont (f a) r) r0

instance Functor ListL where
  fmap f (ListL list) = ListL $ \cont r0 ->
    list (\r a -> cont r (f a)) r0



instance Monad ListR where
  return x = ListR $ \cons nil -> cons x nil
  ListR list >>= f = ListR $ \cons ->
    list (\a r -> case f a of
                    ListR list' -> list' cons r
         )

instance Monad ListL where
  return x = ListL $ \cons nil -> cons nil x
  ListL list >>= f = ListL $ \cons ->
    list (\r a -> case f a of
                    ListL list' -> list' cons r
         )


instance Applicative ListR where
  pure  = return
  (<*>) = ap
instance Applicative ListL where
  pure  = return
  (<*>) = ap

instance Alternative ListR where
  empty = ListR $ \_ r -> r
  ListR contA <|> ListR contB = ListR $ \step r ->
    contA step (contB step r)

instance Alternative ListL where
  empty = ListL $ \_ r -> r
  ListL contA <|> ListL contB = ListL $ \step r ->
    contB step (contA step r)




----------------------------------------------------------------
-- Monadic transformers
----------------------------------------------------------------

-- | Monadic transformer
newtype ListT m a = ListT (forall r. (r -> a -> m r) -> r -> m r)

runListT :: Monad m => ListT m a -> m [a]
runListT (ListT cont) = cont (\as a -> return (as ++ [a])) []

instance Monad m => Functor (ListT m) where
  fmap f (ListT list) = ListT $ \cont r0 -> list (\r a -> cont r (f a)) r0

instance Monad m => Applicative (ListT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ListT m) where
  return x = ListT $ \cons nil -> cons nil x
  ListT list >>= f = ListT $ \cons ->
    list (\r a -> case f a of
                    ListT list' -> list' cons r
         )

instance Monad m => Alternative (ListT m) where
  empty = ListT $ \_ r -> return r
  ListT contA <|> ListT contB = ListT $ \step r ->
    -- NOTE: Left/right fold matters here!
    contB step =<< contA step r


instance MonadTrans ListT where
  lift m = ListT $ \cons nil -> do a <- m
                                   cons nil a

instance MonadIO m => MonadIO (ListT m) where
  liftIO io = do ListT $ \cons nil -> do a <- liftIO io
                                         cons nil a



----------------------------------------------------------------

-- go n = do
--   print $ uncps $ ((arr1 >=> arr2) >=> arr3) n
--   print $ uncps $ (arr1 >=> (arr2 >=> arr3)) n
--   where
--     arr1 n = cps [1..n]
--     arr2 x = cps [x,x*100]
--     arr3 x = cps [x,negate x]
--     cps   = cpsL
--     uncps = uncpsL

-- -- a = 
-- a,b,c :: ListT IO ()
-- [a,b,c] = map (liftIO . putChar) ['a','b','c']
