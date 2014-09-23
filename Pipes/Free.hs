{-# LANGUAGE DeriveFunctor #-}
-- | Reimplementation of pipe's core data type as Free monad
module Pipes.Free where

import Control.Monad
import Control.Monad.Free
import Pipes.Void

----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Bidirectional proxy
type Proxy a' a b' b m r = Free (ProxyF a' a b' b m) r

--                            a' a  b' b
type Producer b   m r = Proxy X  () () b m r
type Pipe     a b m r = Proxy () a  () b m r
type Consumer a   m r = Proxy () a  () X m r


-- | Functor for proxy which is represented as free monad
data ProxyF a' a b' b m r
  = Request a' (a  -> r) -- ^ Proxy blocked on requesting input from
                         --   upstream (await for 1 directional case)
  | Respond b  (b' -> r) -- ^ Proxy blocked on responding to
                         --   downstream (yield for 1 directional case)
  | M       (m r)        -- ^ Carry monadic effect
  deriving (Functor)




----------------------------------------------------------------
-- Push-pull category
----------------------------------------------------------------

-- | Create proxy which is initially blocked on responding
push :: a -> Proxy a' a a' a m r
push a = Free (Respond a pull)

-- | Create proxy which is initially blocked on requesting input from upstream
pull :: a' -> Proxy a' a a' a m r
pull a' = Free (Request a' push)

(>>~)
    :: Functor m
    =>       Proxy a' a b' b m r  -- ^ upstream
    -> (b -> Proxy b' b c' c m r) -- ^ downstream
    ->       Proxy a' a c' c m r  -- ^
Pure r >>~ _  = Pure r
Free p >>~ fb = case p of
  Request a' fa  -> Free $ Request a' (fa >~> fb)
  Respond b  fb' -> fb' +>> fb b
  M          m   -> Free $ M (fmap (>>~ fb) m)

(+>>)
    :: Functor m
    => (b' -> Proxy a' a b' b m r) -- ^ upstream
    ->        Proxy b' b c' c m r  -- ^ downstream
    ->        Proxy a' a c' c m r  -- ^
_   +>> Pure r = Pure r
fb' +>> Free p = case p of
  Request b' fb  -> fb' b' >>~ fb
  Respond c  fc' -> Free $ Respond c (fb' >+> fc')
  M          m   -> Free $ M (fmap (fb' +>>) m)


(>~>) :: Functor m
      => (_a -> Proxy a' a b' b m r) -- ^
      -> ( b -> Proxy b' b c' c m r) -- ^
      -> (_a -> Proxy a' a c' c m r) -- ^
(fa >~> fb) a = fa a >>~ fb

(>+>) :: Functor m
      => ( b' -> Proxy a' a b' b m r) -- ^
      -> (_c' -> Proxy b' b c' c m r) -- ^
      -> (_c' -> Proxy a' a c' c m r) -- ^
(fb' >+> fc') c' = fb' +>> fc' c'



----------------------------------------------------------------
-- Request/responds category
----------------------------------------------------------------

respond :: a -> Proxy x' x a' a m a'
respond a = Free $ Respond a Pure

request :: a' -> Proxy a' a y' y m a
request a' = Free $ Request a' Pure


-- > (//>) ~ for
(//>) :: (Functor m)
      =>       Proxy x' x b' b m a'  -- ^
      -> (b -> Proxy x' x c' c m b') -- ^
      ->       Proxy x' x c' c m a'  -- ^
p0 //> fb = go p0
  where
    go (Pure r) = Pure r
    go (Free p) = case p of
      Request x' fx  -> Free $ Request x' (go . fx)
      Respond b  fb' -> fb b >>= (go . fb')
      M          m   -> Free $ M (fmap go m)


(>\\) :: (Functor m)
      => (b' -> Proxy a' a y' y m b) -- ^
      ->        Proxy b' b y' y m c  -- ^
      ->        Proxy a' a y' y m c  -- ^
fb' >\\ p0     = go p0
  where
    go (Pure r) = Pure r
    go (Free p) = case p of
        Request b' fb  -> fb' b' >>= \b -> go (fb b)
        Respond x  fx' -> Free $ Respond x (\x' -> go (fx' x'))
        M          m   -> Free $ M (fmap go m)


for :: (Functor m)
    =>       Proxy x' x b' b m a'  -- ^
    -> (b -> Proxy x' x c' c m b') -- ^
    ->       Proxy x' x c' c m a'  -- ^
for = (//>)
