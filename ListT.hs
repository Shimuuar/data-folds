module ListT where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class


----------------------------------------------------------------

-- | CPS'd list
newtype List a = List (forall r. (r -> a -> r) -> r -> r)

instance Functor List where
  fmap f (List list) = List $ \cont r0 ->
    list (\r a -> cont r (f a)) r0

instance Monad List where
  return x = List $ \cons nil -> cons nil x
  List list >>= f = List $ \cons ->
    list (\r a -> case f a of
                    List list' -> list' cons r
         )

instance Applicative List where
  pure  = return
  (<*>) = ap

instance Alternative List where
  empty = List $ \_ r -> r
  List contA <|> List contB = List $ \step r ->
    -- NOTE: Left/right fold matters here!
    contB step (contA step r)


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

cpsR,cpsL :: [a] -> List a
cpsR xs = List $ \cons nil -> foldr (flip cons) nil xs
cpsL xs = List $ \step x0  -> foldl step x0 xs

uncpsR,uncpsL :: List a -> [a]
uncpsR (List list) = list (flip (:)) []
uncpsL (List list) = list (\xs x -> xs ++ [x]) [] -- FIXME: diff lists


go n = do
  print $ uncps $ ((arr1 >=> arr2) >=> arr3) n
  print $ uncps $ (arr1 >=> (arr2 >=> arr3)) n
  where
    arr1 n = cps [1..n]
    arr2 x = cps [x,x*100]
    arr3 x = cps [x,negate x]
    cps   = cpsL
    uncps = uncpsL



-- a = 
a,b,c :: ListT IO ()
[a,b,c] = map (liftIO . putChar) ['a','b','c']
