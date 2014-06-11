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

import Data.Folds.Class
import Data.Folds.Pipette
import Data.Folds.Left
import Data.Folds.Monadic
