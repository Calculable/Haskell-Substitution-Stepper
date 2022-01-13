{-# OPTIONS -XNoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : SteppablePrelude
-- Description : Definitions of standart Haskell types, type classes, type class instances and functions
-- License     : GPL-3
--
-- This customized version of the Haskell Standard prelude is loaded into the stepper
-- together with the input Haskell file defined by the user. It means that users
-- can work with existing types, type classes and functions from the prelude without
-- having to define everything by themselves.
--
-- Source: most of the source code is taken from the Haskell Report 2010 Standard Prelude
module SteppablePrelude (module SteppablePrelude, module GHC.Maybe, module Data.Char, module Prelude) where

{-Imports-}

--import only "unsteppable functionality", provide the other functions
import Data.Char (isSpace, ord)
import GHC.Maybe hiding (Maybe (..))
import Prelude
  ( Applicative (..),
    Bool (..),
    Bounded (..),
    Char (..),
    Double (..),
    Eq (..),
    Float (..),
    Floating (..),
    Fractional (..),
    Functor (..),
    Int (..),
    Integer (..),
    Integral (..),
    Monad (..),
    Num (..),
    Ord (..),
    Rational (..),
    Real (..),
    RealFloat (..),
    RealFrac (..),
    String (..),
    abs,
    acos,
    acosh,
    asin,
    asinh,
    atan,
    atan2,
    atanh,
    ceiling,
    cos,
    cosh,
    decodeFloat,
    div,
    divMod,
    encodeFloat,
    error,
    exp,
    exponent,
    fail,
    floatDigits,
    floatRadix,
    floatRange,
    floor,
    fmap,
    fromInteger,
    isDenormalized,
    isIEEE,
    isInfinite,
    isNaN,
    isNegativeZero,
    log,
    logBase,
    max,
    min,
    mod,
    negate,
    properFraction,
    quot,
    quotRem,
    recip,
    rem,
    return,
    round,
    scaleFloat,
    seq,
    significand,
    signum,
    sin,
    sinh,
    sqrt,
    tan,
    tanh,
    toInteger,
    toRational,
    truncate,
    (*),
    (**),
    (+),
    (-),
    (/),
    (/=),
    (<),
    (<=),
    (==),
    (>),
    (>=),
    (>>),
    (>>=),
  )

{-Infixr-}
infixr 9 .

infixr 8 ^, ^^

infixr 3 &&

infixr 2 ||

infixr 1 =<<

infixr 0 $, $!

infixl 9 !!

infixr 5 ++

infix 4 `elem`, `notElem`

{-Type: Maybe-}
data Maybe a = Nothing | Just a deriving (Eq, Ord)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing = n
maybe n f (Just x) = f x

instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Monad Maybe where
  (Just x) >>= k = k x
  Nothing >>= k = Nothing
  return = Just

instance Applicative Maybe where
  pure = Just
  Just f <*> Just x = Just (f x)

--Maybe functions taken from Data.Maybe
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

fromMaybe :: a -> Maybe a -> a
fromMaybe d x = case x of Nothing -> d; Just v -> v

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe = foldr (const . Just) Nothing

catMaybes :: [Maybe a] -> [a]
catMaybes = mapMaybe id

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) =
  let rs = mapMaybe f xs
   in case f x of
        Nothing -> rs
        Just r -> r : rs

mapMaybeFB :: (b -> r -> r) -> (a -> Maybe b) -> a -> r -> r
mapMaybeFB cons f x next = case f x of
  Nothing -> next
  Just r -> cons r next

{-Type: Ordering-}
data Ordering = LT | EQ | GT
  deriving (Eq, Ord, Bounded)

{-Type: Either-}
data Either a b = Left a | Right b deriving (Eq, Ord)

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x) = f x
either f g (Right y) = g y

instance Functor (Either a) where
  fmap _ (Left x) = Left x
  fmap f (Right y) = Right (f y)

instance Applicative (Either e) where
  pure = Right
  Left e <*> _ = Left e
  Right f <*> r = fmap f r

instance Monad (Either e) where
  Left l >>= _ = Left l
  Right r >>= k = k r

--Either functions taken from Data.Either

lefts :: [Either a b] -> [a]
lefts x = [a | Left a <- x]

rights :: [Either a b] -> [b]
rights x = [a | Right a <- x]

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
  where
    left a ~(l, r) = (a : l, r)
    right a ~(l, r) = (l, a : r)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _ = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _ = b

{-Functions: Tuple-}
fst :: (a, b) -> a
fst (x, y) = x

snd :: (a, b) -> b
snd (x, y) = y

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p = f (fst p) (snd p)

{-Functions: Numeric-}
subtract :: (Num a) => a -> a -> a
subtract = flip (-)

even, odd :: (Integral a) => a -> Bool
even n = n `rem` 2 == 0
odd = not . even

gcd :: (Integral a) => a -> a -> a
gcd 0 0 = error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y = gcd' (abs x) (abs y)
  where
    gcd' x 0 = x
    gcd' x y = gcd' y (x `rem` y)

lcm :: (Integral a) => a -> a -> a
lcm _ 0 = 0
lcm 0 _ = 0
lcm x y = abs ((x `quot` (gcd x y)) * y)

(^) :: (Num a, Integral b) => a -> b -> a
x ^ 0 = 1
x ^ n | n > 0 = f x (n - 1) x
  where
    f _ 0 y = y
    f x n y = g x n
      where
        g x n
          | even n = g (x * x) (n `quot` 2)
          | otherwise = f x (n - 1) (x * y)
_ ^ _ = error "Prelude.^: negative exponent"

(^^) :: (Fractional a, Integral b) => a -> b -> a
x ^^ n = if n >= 0 then x ^ n else recip (x ^ (-n))

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger

realToFrac :: (Real a, Fractional b) => a -> b
realToFrac = fromRational . toRational

{-Functions: General-}
id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

($), ($!) :: (a -> b) -> a -> b
f $ x = f x
f $! x = x `seq` f x

{-Functions: On Boolean-}
(&&), (||) :: Bool -> Bool -> Bool
True && x = x
False && _ = False
True || _ = True
False || x = x

not :: Bool -> Bool
not True = False
not False = True

otherwise :: Bool
otherwise = True

{-Functions: Misc-}
until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x
  | p x = x
  | otherwise = until p f (f x)

asTypeOf :: a -> a -> a
asTypeOf = const

undefined :: a
undefined = error "Prelude.undefined"

{-Functions: List-}
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x : xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

concat :: [[a]] -> [a]
concat xss = foldr (++) [] xss

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

head :: [a] -> a
head (x : _) = x
head [] = error "Prelude.head: empty list"

tail :: [a] -> [a]
tail (_ : xs) = xs
tail [] = error "Prelude.tail: empty list"

last :: [a] -> a
last [x] = x
last (_ : xs) = last xs
last [] = error "Prelude.last: empty list"

init :: [a] -> [a]
init [x] = []
init (x : xs) = x : init xs
init [] = error "Prelude.init: empty list"

null :: [a] -> Bool
null [] = True
null (_ : _) = False

length :: [a] -> Int
length [] = 0
length (_ : l) = 1 + length l

(!!) :: [a] -> Int -> a
xs !! n | n < 0 = error "Prelude.!!: negative index"
[] !! _ = error "Prelude.!!: index too large"
(x : _) !! 0 = x
(_ : xs) !! n = xs !! (n - 1)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x : xs) = foldl f (f z x) xs

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x : xs) = foldl f x xs
foldl1 _ [] = error "Prelude.foldl1: empty list"

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q xs =
  q :
  ( case xs of
      [] -> []
      x : xs -> scanl f (f q x) xs
  )

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x : xs) = scanl f x xs
scanl1 _ [] = []

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x : xs) = f x (foldr f z xs)

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x] = x
foldr1 f (x : xs) = f x (foldr1 f xs)
foldr1 _ [] = error "Prelude.foldr1: empty list"

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f q0 [] = [q0]
scanr f q0 (x : xs) = f x q : qs
  where
    qs@(q : _) = scanr f q0 xs

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 f [] = []
scanr1 f [x] = [x]
scanr1 f (x : xs) = f x q : qs
  where
    qs@(q : _) = scanr1 f xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

repeat :: a -> [a]
repeat x = xs where xs = x : xs

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

cycle :: [a] -> [a]
cycle [] = error "Prelude.cycle: empty list"
cycle xs = xs' where xs' = xs ++ xs'

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_ : xs) = drop (n - 1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p xs@(x : xs')
  | p x = dropWhile p xs'
  | otherwise = xs

span, break :: (a -> Bool) -> [a] -> ([a], [a])
span p [] = ([], [])
span p xs@(x : xs')
  | p x = (x : ys, zs)
  | otherwise = ([], xs)
  where
    (ys, zs) = span p xs'
break p = span (not . p)

lines :: String -> [String]
lines "" = []
lines s =
  let (l, s') = break (== '\n') s
   in l : case s' of
        [] -> []
        (_ : s'') -> lines s''

words :: String -> [String]
words s = case dropWhile isSpace s of
  "" -> []
  s' -> w : words s''
    where
      (w, s'') = break isSpace s'

unlines :: [String] -> String
unlines = concatMap (++ "\n")

unwords :: [String] -> String
unwords [] = ""
unwords ws = foldr1 (\w s -> w ++ ' ' : s) ws

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

and, or :: [Bool] -> Bool
and = foldr (&&) True
or = foldr (||) False

any, all :: (a -> Bool) -> [a] -> Bool
any p = or . map p
all p = and . map p

elem, notElem :: (Eq a) => a -> [a] -> Bool
elem x = any (== x)
notElem x = all (/= x)

lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup key [] = Nothing
lookup key ((x, y) : xys)
  | key == x = Just y
  | otherwise = lookup key xys

sum, product :: (Num a) => [a] -> a
sum = foldl (+) 0
product = foldl (*) 1

maximum, minimum :: (Ord a) => [a] -> a
maximum [] = error "Prelude.maximum: empty list"
maximum xs = foldl1 max xs
minimum [] = error "Prelude.minimum: empty list"
minimum xs = foldl1 min xs

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 = zipWith3 (,,)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith z (a : as) (b : bs) =
  z a b : zipWith z as bs
zipWith _ _ _ = []

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 z (a : as) (b : bs) (c : cs) =
  z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _ = []

unzip :: [(a, b)] -> ([a], [b])
unzip = foldr (\(a, b) ~(as, bs) -> (a : as, b : bs)) ([], [])

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 =
  foldr
    (\(a, b, c) ~(as, bs, cs) -> (a : as, b : bs, c : cs))
    ([], [], [])

{- Functions: monadic-}
sequence :: Monad m => [m a] -> m [a]
sequence = foldr mcons (return [])
  where
    mcons p q = p >>= \x -> q >>= \y -> return (x : y)

sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as = sequence (map f as)

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as = sequence_ (map f as)

(=<<) :: Monad m => (a -> m b) -> m a -> m b
f =<< x = x >>= f

{-Type Class: Enum-}
class Enum a where
  succ, pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a] -- [n..]
  enumFromThen :: a -> a -> [a] -- [n,n'..]
  enumFromTo :: a -> a -> [a] -- [n..m]
  enumFromThenTo :: a -> a -> a -> [a] -- [n,n'..m]

  succ = toEnum . (+ 1) . fromEnum
  pred = toEnum . (subtract 1) . fromEnum
  enumFrom x = map toEnum [fromEnum x ..]
  enumFromTo x y = map toEnum [fromEnum x .. fromEnum y]
  enumFromThen x y = map toEnum [fromEnum x, fromEnum y ..]
  enumFromThenTo x y z =
    map toEnum [fromEnum x, fromEnum y .. fromEnum z]

instance Enum Char where
  pred = toEnum . (subtract 1) . fromEnum
  succ = toEnum . (+ 1) . fromEnum
  toEnum x = unsteppableFunction'primIntToChar x
  fromEnum x = unsteppableFunction'primCharToInt x
  enumFrom = customEnumFrom
  enumFromTo = customEnumFromTo
  enumFromThen = customEnumFromThen
  enumFromThenTo = customEnumFromThenTo

instance Enum Int where
  succ x = x + 1
  pred x = x - 1
  toEnum x = x
  fromEnum x = x
  enumFrom = customEnumFrom
  enumFromTo = customEnumFromTo
  enumFromThen = customEnumFromThen
  enumFromThenTo = customEnumFromThenTo

instance Enum Integer where
  succ x = x + 1
  pred x = x - 1
  toEnum x = fromIntegral x
  fromEnum x = fromInteger x
  enumFrom = customEnumFrom
  enumFromTo = customEnumFromTo
  enumFromThen = customEnumFromThen
  enumFromThenTo = customEnumFromThenTo

instance Enum Float where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromIntegral
  fromEnum = fromInteger . truncate -- may overflow
  enumFrom = customEnumFrom
  enumFromTo = customEnumFromTo
  enumFromThen = customEnumFromThen
  enumFromThenTo = customEnumFromThenTo

instance Enum Double where
  succ x = x + 1
  pred x = x - 1
  toEnum = fromIntegral
  fromEnum = fromInteger . truncate -- may overflow
  enumFrom = customEnumFrom
  enumFromTo = customEnumFromTo
  enumFromThen = customEnumFromThen
  enumFromThenTo = customEnumFromThenTo

instance Enum Bool where
  succ False = True
  succ True = error "Prelude.Enum.Bool.succ: bad argument"
  pred True = False
  pred False = error "Prelude.Enum.Bool.succ: bad argument"
  fromEnum True = 1
  fromEnum False = 0
  toEnum 1 = True
  toEnum 0 = False
  enumFrom True = [True]
  enumFrom False = [False, True]
  enumFromTo True True = [True]
  enumFromTo True False = []
  enumFromTo False True = [False, True]
  enumFromTo False False = [False]
  enumFromThen True True = True : enumFromThen True True
  enumFromThen True False = [True, False]
  enumFromThen False True = [False, True]
  enumFromThen False False = False : enumFromThen False False
  enumFromThenTo True True True = True : enumFromThenTo True True True
  enumFromThenTo True True False = []
  enumFromThenTo True False True = [True]
  enumFromThenTo True False False = [True, False]
  enumFromThenTo False True True = [False, True]
  enumFromThenTo False True False = [False]
  enumFromThenTo False False True = False : False : enumFromThenTo False False True
  enumFromThenTo False False False = False : enumFromThenTo False False False

customEnumFrom :: Enum a => a -> [a]
customEnumFrom x = x : customEnumFrom (succ x)

customEnumFromTo :: (Enum a, Ord a) => a -> a -> [a]
customEnumFromTo n m = takeWhile (<= m) (customEnumFrom n) --check: why is original prelude implementation criteria (<= m+1/2)

customEnumFromThen :: (Enum a) => a -> a -> [a]
customEnumFromThen n m = n : customEnumFromThen (succStep n (stepSize n m)) (succStep m (stepSize n m))

customEnumFromThenTo :: (Enum a, Ord a) => a -> a -> a -> [a]
customEnumFromThenTo n n' m =
  if n > m
    then []
    else n : customEnumFromThenTo (succStep n (stepSize n n')) (succStep n' (stepSize n n')) m

succStep :: (Enum a) => a -> Int -> a
succStep enum stepsize = toEnum (fromEnum enum + stepsize)

stepSize :: (Enum a) => a -> a -> Int
stepSize x y = fromEnum y - fromEnum x

{-Functions that are implemented in the backend. The definitions here are not actualy used in the stepping process but simply added here so there are no compilation errors for this file. Every function that is prefixed with "unsteppableFunction" is not reduced using the definition from this file but with the definition in the Haskell backend-}
unsteppableFunction'primIntToChar :: Int -> Char
unsteppableFunction'primIntToChar _ = error "This is only a dummy function. The real reduction happens in the stepper backend"

unsteppableFunction'primCharToInt :: Char -> Int
unsteppableFunction'primCharToInt _ = error "This is only a dummy function. The real reduction happens in the stepper backend"
