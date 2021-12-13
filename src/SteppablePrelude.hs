{-# OPTIONS -XNoImplicitPrelude #-}

module SteppablePrelude where
import Prelude hiding (subtract, even, odd, gcd, lcm, (^), (^^), fromIntegral, realToFrac, id, const, (.), flip, ($), ($!))

-- Numeric functions


subtract         :: (Num a) => a -> a -> a
subtract         =  flip (-)


even, odd        :: (Integral a) => a -> Bool
even n           =  n `rem` 2 == 0
odd              =  not . even


gcd              :: (Integral a) => a -> a -> a
gcd 0 0          =  error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y          =  gcd' (abs x) (abs y)
                    where gcd' x 0  =  x
                          gcd' x y  =  gcd' y (x `rem` y)


lcm              :: (Integral a) => a -> a -> a
lcm _ 0          =  0
lcm 0 _          =  0
lcm x y          =  abs ((x `quot` (gcd x y)) * y)


(^)              :: (Num a, Integral b) => a -> b -> a
x ^ 0            =  1
x ^ n | n > 0    =  f x (n-1) x
                    where f _ 0 y = y
                          f x n y = g x n  where
                                    g x n | even n  = g (x*x) (n `quot` 2)
                                          | otherwise = f x (n-1) (x*y)
_ ^ _            = error "Prelude.^: negative exponent"


(^^)             :: (Fractional a, Integral b) => a -> b -> a
x ^^ n           =  if n >= 0 then x^n else recip (x^(-n))


fromIntegral     :: (Integral a, Num b) => a -> b
fromIntegral     =  fromInteger . toInteger


realToFrac     :: (Real a, Fractional b) => a -> b
realToFrac      =  fromRational . toRational



-- Function type

-- identity function

id               :: a -> a
id x             =  x

-- constant function

const            :: a -> b -> a
const x _        =  x

-- function composition

(.)              :: (b -> c) -> (a -> b) -> a -> c
f . g            =  \ x -> f (g x)

-- flip f  takes its (first) two arguments in the reverse order of f.

flip             :: (a -> b -> c) -> b -> a -> c
flip f x y       =  f y x

-- right-associating infix application operators 
-- (useful in continuation-passing style)

($), ($!) :: (a -> b) -> a -> b
f $  x    =  f x
f $! x    =  x `seq` f x



