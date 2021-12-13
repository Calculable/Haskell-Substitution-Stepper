{-# LANGUAGE GADTs #-}

module SafeListSouceExample where

data Empty

data NonEmpty

data SafeList a b where
  Nil :: SafeList a Empty
  Cons :: a -> SafeList a b -> SafeList a NonEmpty

instance Show a => Show (SafeList a b) where
  show Nil = "Nil"
  show (Cons x xs) = show x ++ ", " ++ show xs

safeHead :: SafeList a NonEmpty -> a
safeHead (Cons x _) = x
