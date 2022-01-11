{-# OPTIONS -XNoImplicitPrelude #-}

module UsageOfPreludeExample where

import SteppablePrelude

x = reverse [1, 2, 3] --functions from the prelude can be used
y = Left 5 == Right 5 --data types from the prelude can be used