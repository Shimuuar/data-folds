-- | Experiments with streams
module Streams where

import Control.Monad.Cont.ListT
import Data.Folds.Left
import Data.Folds.Monadic


----------------------------------------------------------------
-- Pure
----------------------------------------------------------------

-- | Infinite stream
data Stream a = a :> Stream a

-- | Convert left fold to stream transformation.
--
--   N.B. we have two choices. We can either extract first element of
--   target stream from initial state of fold or only generate new
--   element by applying step function. It's not yet clear which
--   choice is correct.
foldToStream :: Fold a b -> Stream a -> Stream b
foldToStream (Fold step x out) (a :> xs)
  = out x :> foldToStream (Fold step (step x a) out) xs

-- | We can compose folds.
composeFold :: Fold a b -> Fold b c -> Fold a c
composeFold (Fold stepAB xA0 outA) (Fold stepBC xB0 outB)
  = Fold
      (\(xA,xB) a -> let xA' = stepAB xA a
                         b   = outA xA' -- !!!
                         xB' = stepBC xB b
                     in (xA',xB')
      )
      (xA0,xB0)
      (\(_,xB) -> outB xB)

-- | We can compose folds.
composeFold2 :: Fold a b -> Fold b c -> Fold a c
composeFold2 (Fold stepAB xA0 outA) (Fold stepBC xB0 outB)
  = Fold
      (\(xA,xB) a -> let xA' = stepAB xA a
                         b   = outA xA -- !!!
                         xB' = stepBC xB b
                     in (xA',xB')
      )
      (xA0,xB0)
      (\(_,xB) -> outB xB)



----------------------------------------------------------------
-- Monadic
----------------------------------------------------------------

-- | Infinite monadic stream
data StreamM m a = m a :+ m (StreamM m a)

streamMToListT :: Monad m => StreamM m a -> ListT m a
streamMToListT s = ListT (go s)
  where
    go (ma :+ rest) step r0 = do
      a  <- ma
      r  <- step r0 a
      ms <- rest
      go ms step r
