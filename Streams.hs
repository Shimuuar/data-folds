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

-- | Convert left fold to stream transformation
foldToStream :: Fold a b -> Stream a -> Stream b
foldToStream (Fold step x out) (a :> xs)
  = out x :> foldToStream (Fold step (step x a) out) xs



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
