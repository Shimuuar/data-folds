{-# LANGUAGE DeriveFunctor #-}
-- | Reduction of ListT definition from pipes. We will not use free
--   monad formulation for the pipes because monad instance for the
--   ListT is different
module Pipes.ListT where

import Control.Applicative
import Control.Monad.Free

-- | Let first get rid of newtypes
--
-- > ListT m a ~ Producer m a ()
-- >           ~ Proxy X () () a m ()
--
-- Here is definition of Proxy data type
--
-- > data Proxy a' a b' b m r
-- >     = Request a' (a  -> Proxy a' a b' b m r )
-- >     | Respond b  (b' -> Proxy a' a b' b m r )
-- >     | M          (m    (Proxy a' a b' b m r))
-- >     | Pure    r
--
-- First of all Request constructor is uninhabited so we can write
-- ListT data type as


----------------------------------------------------------------
-- First step
--
-- Same data type but with simplified constructors
----------------------------------------------------------------

type ListT1 m a = Free (LF1 m a) ()

data LF1 m a r
  = Yield1 a (() -> r)
  | M1 (m r)
  deriving (Functor)

yield1 :: a -> ListT1 m a
yield1 a = Free $ Yield1 a Pure

for1 :: Functor m => ListT1 m a -> (a -> ListT1 m b) -> ListT1 m b
for1 p0 fb = go p0
  where
    go (Pure r) = Pure r
    go (Free p) = case p of
      Yield1 a  fb' -> fb a >>= (go . fb')
      M1        m   -> Free $ M1 (fmap go m)


----------------------------------------------------------------
-- We can remove dummy parameter from Yield's second field
----------------------------------------------------------------

type ListT2 m a = Free (LF2 m a) ()

data LF2 m a r
  = Yield2 a r
  | M2    (m r)
  deriving (Functor)

yield2 :: a -> ListT2 m a
yield2 a = Free $ Yield2 a (Pure ())

for2 :: Functor m => ListT2 m a -> (a -> ListT2 m b) -> ListT2 m b
for2 p0 fb = go p0
  where
    go (Pure _) = Pure ()
    go (Free p) = case p of
      Yield2 a  fb' -> fb a >> (go fb')
      M2        m   -> Free $ M2 (fmap go m)


----------------------------------------------------------------
-- Now we want to get rid from Free monad. So here is reminder on how
-- does monadic bind for free monad works
--
-- > Pure a >>= f = f a
-- > Free m >>= f = Free ((>>= f) <$> m)
--
-- Consequently (m >> k ≡ m >>= (\_ -> m)
--
-- > Pure _ >> m = m
-- > Free m >> h = Free ((>> h) <$> m)
--
-- Now we can get rid of Free monad
----------------------------------------------------------------

data ListT3 m a = Yield3 a (ListT3 m a)
                | M3 (m (ListT3 m a))
                | Pure3

yield3 :: a -> ListT3 m a
yield3 a = Yield3 a Pure3

for3 :: Functor m => ListT3 m a -> (a -> ListT3 m b) -> ListT3 m b
for3 p0 fb = go p0
  where
    go  Pure3       = Pure3
    go (Yield3 a m) = fb a `seql` go m
    go (M3 m)       = M3 (fmap go m)

seql :: Functor m => ListT3 m a -> ListT3 m a -> ListT3 m a
seql Pure3        m = m
seql (Yield3 a m) h = Yield3 a (seql m h)
seql (M3 m)       h = M3 (fmap (`seql` h) m)



-- Now this definition moderately resemble one from list-t package
