{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -XNoImplicitPrelude #-}

module TypeFamilySourceExample where

import SteppablePrelude

type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where
  Append '[] xs = xs
  Append (x : xs) ys = x : Append xs ys

type X = Append [1, 2] [3, 4]
