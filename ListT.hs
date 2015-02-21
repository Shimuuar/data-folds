{-# LANGUAGE RankNTypes #-}
module ListT (
    -- * Pure CPS-list
    ListL(..)
  , ListR(..)
  , cpsR
  , cpsL
  , uncpsR
  , uncpsL
    -- ** List transformations
  , consR
  , consL
  , snocR
  , snocL
    -- * List functions
  , headR
  , tailL
  , takeL
  , scanlL
  , unfoldrR
  , unfoldrL
    -- * Monad transformer
  , ListT(..)
  , runListT
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Monoid
import qualified Data.Foldable    as T
import qualified Data.Traversable as T


----------------------------------------------------------------
-- CPS lists
----------------------------------------------------------------

-- | CPS-transformation of list which should be consumed using right
--   fold.
newtype ListR a = ListR { foldrR :: forall r. (a -> r -> r) -> r -> r }

-- | CPS-transformation of list which should be consumed using left
--   fold.
newtype ListL a = ListL { foldlL :: forall r. (r -> a -> r) -> r -> r }

instance Show a => Show (ListR a) where
  show = show . uncpsR
instance Show a => Show (ListL a) where
  show = show . uncpsL


-- | Convert list to CPS form using right fold
cpsR :: [a] -> ListR a
cpsR xs = ListR $ \cons nil -> foldr cons nil xs

-- | Convert list to CPS form using right fold
cpsL :: [a] -> ListL a
cpsL xs = ListL $ \step x0 -> foldl step x0 xs


uncpsR :: ListR a -> [a]
uncpsR (ListR list) = list (:) []

uncpsL :: ListL a -> [a]
uncpsL (ListL list) = list (\xs x -> xs . (x:)) id []

-- | Prepend element to a list
consR :: a -> ListR a -> ListR a
consR a xs = ListR $ \cons nil -> cons a $ foldrR xs cons nil

-- | Prepend element to a list
consL :: a -> ListL a -> ListL a
consL a xs = ListL $ \step x0 -> foldlL xs step (step x0 a)

snocR :: ListR a -> a -> ListR a
snocR xs a = ListR $ \cons nil -> foldrR xs cons (cons a nil)

-- | Append element to a list
snocL :: ListL a -> a -> ListL a
snocL xs a = ListL $ \step x0 -> step (foldlL xs step x0) a




----------------------------------------------------------------
-- Instances
----------------------------------------------------------------


----------------------------------------
instance Functor ListR where
  fmap f (ListR list) = ListR $ \cons nil ->
    list (\a as -> f a `cons` as) nil

instance Functor ListL where
  fmap f (ListL list) = ListL $ \step x0 ->
    list (\r a -> step r (f a)) x0


----------------------------------------
instance Applicative ListR where
  pure  = return
  (<*>) = ap

instance Applicative ListL where
  pure  = return
  (<*>) = ap


----------------------------------------
instance Monad ListR where
  return x = ListR $ \cons nil -> cons x nil
  ListR list >>= f = ListR $ \cons nil ->
    list (\a r -> foldrR (f a) cons r) nil

instance Monad ListL where
  return a = ListL $ \step x0 -> step x0 a
  ListL list >>= f = ListL $ \step x0 ->
    list (\r a -> foldlL (f a) step r) x0


----------------------------------------
instance Alternative ListR where
  empty = ListR $ \_ r -> r
  ListR contA <|> ListR contB = ListR $ \cons nil ->
    contA cons (contB cons nil)

instance Alternative ListL where
  empty = ListL $ \_ r -> r
  ListL contA <|> ListL contB = ListL $ \step x0 ->
    contB step (contA step x0)

instance Monoid (ListR a) where
  mempty  = empty
  mappend = (<|>)
instance Monoid (ListL a) where
  mempty  = empty
  mappend = (<|>)

instance T.Foldable ListR where
  foldr f x l = foldrR l f x

----------------------------------------------------------------
-- Functions
----------------------------------------------------------------

headL :: ListL a -> Maybe a
headL = undefined

headR :: ListR a -> Maybe a
headR xs = foldrR xs (\a _ -> Just a) Nothing

tailL :: ListL a -> ListL a
tailL list = ListL $ \step x0 ->
  snd $ foldlL list (\(f,r) a -> if f then (False,r) else (False,step r a)) (True,x0)

instance T.Foldable ListL where
  foldMap f l = foldlL (fmap f l) mappend mempty

instance T.Traversable ListR where
  sequenceA l = foldrR l (liftA2 consR) (pure mempty)

instance T.Traversable ListL where
  sequenceA l = foldlL l (liftA2 snocL) (pure mempty)

takeL :: Int -> ListL a -> ListL a
-- FIXME: it traverse list fully.
takeL n0 xs = ListL $ \step r0 ->
  snd $ foldlL xs
    (\(n,r) a -> if n <= 0 then (n,r) else (n-1,step r a))
    (n0,r0)




scanlL :: (b -> a -> b) -> b -> ListL a -> ListL b
scanlL f b0 list = ListL $ \step x0 ->
  snd $ foldlL list (\(b,r) a -> let b' = f b a in (b',step r b')) (b0,step x0 b0)

unfoldrR :: (s -> Maybe (a,s)) -> s -> ListR a
unfoldrR f s0 = ListR $ \cons nil ->
  let loop s = case f s of
                 Just (a,s') -> cons a (loop s')
                 Nothing     -> nil
  in loop s0

unfoldrL :: (s -> Maybe (a,s)) -> s -> ListL a
unfoldrL f s0 = ListL $ \step r0 ->
  let loop r s = case f s of
                   Just (a,s') -> loop (step r a) s'
                   Nothing     -> r
  in loop r0 s0


----------------------------------------------------------------
-- Monadic transformers
----------------------------------------------------------------

-- | Monadic transformer
newtype ListT m a = ListT { foldMT :: forall r. (r -> a -> m r) -> r -> m r }

runListT :: Monad m => ListT m a -> m [a]
runListT list = foldMT list (\as a -> return (as ++ [a])) []

instance Monad m => Functor (ListT m) where
  fmap f (ListT list) = ListT $ \cont r0 -> list (\r a -> cont r (f a)) r0

instance Monad m => Applicative (ListT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ListT m) where
  return x = ListT $ \cons nil -> cons nil x
  list >>= f = ListT $ \cons ->
    foldMT list (\r a -> foldMT (f a) cons r)


-- FIXME: Is this meaningful instance
instance Monad m => Alternative (ListT m) where
  empty = ListT $ \_ r -> return r
  ListT contA <|> ListT contB = ListT $ \step r ->
    -- NOTE: Left/right fold matters here!
    contB step =<< contA step r


instance MonadTrans ListT where
  lift m = ListT $ \cons nil -> cons nil =<< m

instance MonadIO m => MonadIO (ListT m) where
  liftIO io = ListT $ \cons nil -> cons nil =<< liftIO io


fromListL :: Monad m => ListL a -> ListT m a
fromListL (ListL list) = ListT $ \step r0 ->
  list (\mr a -> do r <- mr
                    step r a
       ) (return r0)

fromListR :: Monad m => ListR a -> ListT m a
fromListR (ListR list) = ListT $ \step r0 ->
  list (\a k r -> step r a >>= k) return r0

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
-- aa,bb,cc :: ListT IO ()
-- [aa,bb,cc] = map (liftIO . putChar) ['a','b','c']
