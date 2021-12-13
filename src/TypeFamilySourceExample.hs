{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module TypeFamilySourceExample where

type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where
    Append '[] xs = xs
    Append (x:xs) ys = x : Append xs ys

type X = Append [1,2] [3,4]

