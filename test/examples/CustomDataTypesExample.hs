module CustomDataTypesExample where

-- Custom List
data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show, Read)

l :: List Integer
l = Cons 1 (Cons 2 (Cons 3 Nil))

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: a -> List a -> List a
append x (Cons y Nil) = Cons y (Cons x Nil)
append x (Cons y ys) = Cons y (append x ys)

rev :: List a -> List a
rev Nil = Nil
rev a@(Cons x Nil) = a
rev (Cons x xs) = append x (rev xs)

-- Custom Maybe
data Optional a = None | Some a deriving (Eq, Ord, Show, Read)

instance Functor Optional where
  fmap _ None = None
  fmap f (Some x) = Some (f x)

instance Applicative Optional where
  pure = Some
  None <*> _ = None
  (Some f) <*> g = fmap f g

instance Monad Optional where
  return = pure
  None >>= g = None
  (Some x) >>= f = f x

isSome :: Optional a -> Bool
isSome None = False
isSome _ = True

isNone :: Optional a -> Bool
isNone = not . isSome
