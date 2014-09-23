-- | Uninhabited data type
module Pipes.Void where

-- | Uninhabited data type
data X

absurd :: X -> a
absurd _ = error "Absurd"
