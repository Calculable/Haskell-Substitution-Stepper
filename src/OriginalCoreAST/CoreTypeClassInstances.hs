module OriginalCoreAST.CoreTypeClassInstances()
where

import GHC.Core (Expr (..))
import GHC.Types.Literal(Literal (..))
import OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression, charToCoreLiteral, rationalToCoreLiteral)
import GHC.Float (rationalToDouble)
import Refact.Compat (xFlags)

instance Num (Expr b) where
  (+) (Lit x) (Lit y) = Lit ((+) x y)
  (+) _ _ = error "+ not supported by this type"
  (-) (Lit x) (Lit y) = Lit ((-) x y)
  (-) _ _ = error "- not supported by this type"
  (*) (Lit x) (Lit y) = Lit ((*) x y)
  (*) _ _ = error "* not supported by this type"
  signum (Lit x) = Lit (signum x)
  signum _ = error "signum not supported by this type"
  fromInteger x = integerToCoreExpression x
  abs (Lit x) = Lit (abs x)
  abs _ = error "abs not supported by this type"

instance Fractional (Expr b) where
  (/) (Lit x) (Lit y) = Lit ((/) x y)
  (/) _ _ = error "/ not supported by this type"
  recip (Lit x) = Lit (recip x)
  recip _ = error "recip not supported by this type"
  fromRational x = rationalToCoreExpression x

instance Eq (Expr b) where
  (/=) (Lit x) (Lit y) = (/=) x y
  (/=) _ _ = error "/= not supported by this type"
  (==) (Lit x) (Lit y) = (==) x y
  (==) _ _ = error "== not supported by this type"

instance Ord (Expr b) where
  (<=) (Lit x) (Lit y) = (<=) x y
  (<=) _ _ = error "<= not supported by this type"
  (<) (Lit x) (Lit y) = (<=) x y
  (<) _ _ = error "< not supported by this type"
  (>=) (Lit x) (Lit y) = (<=) x y
  (>=) _ _ = error ">= not supported by this type"
  (>) (Lit x) (Lit y) = (<=) x y
  (>) _ _ = error "> not supported by this type"

instance Enum (Expr b) where
  succ (Lit x) = Lit (succ x)
  succ _ = error "succ not supported for this type"

  fromEnum (Lit x) = (fromEnum x)
  fromEnum _ = error "fromEnum not supported for this type"

  enumFrom _ = error "enumFrom not supported for this type" 
  enumFromThen _ = error "enumFromThen not supported for this type" 
  enumFromTo _ = error "enumFromTo not supported for this type" 
  enumFromThenTo _ = error "enumFromThenTo not supported for this type"
  
  toEnum x = Lit (integerToCoreLiteral (toInteger x))

instance Floating (Expr b) where
  pi = Lit (fractionalToCoreLiteral Prelude.pi)
  
  exp (Lit x) = Lit (exp x)
  exp _ = error "exp not supported for this type"

  log (Lit x) = Lit (log x)
  log _ = error "log not supported for this type"
  
  sqrt (Lit x) = Lit (sqrt x)
  sqrt _ = error "sqrt not supported for this type"

  (**) (Lit x) (Lit y) = Lit ((**) x y)
  (**) _ _ = error "(**) not supported for this type"

  logBase (Lit x) (Lit y) = Lit (logBase x y)
  logBase _ _ = error "logBase not supported for this type"

  sin (Lit x) = Lit (sin x)
  sin _ = error "sin not supported for this type"

  cos (Lit x) = Lit (cos x)
  cos _ = error "cos not supported for this type"

  tan (Lit x) = Lit (tan x)
  tan _ = error "tan not supported for this type"

  asin (Lit x) = Lit (asin x)
  asin _ = error "asin not supported for this type"

  acos (Lit x) = Lit (acos x)
  acos _ = error "acos not supported for this type"

  atan (Lit x) = Lit (atan x)
  atan _ = error "atan not supported for this type"

  sinh (Lit x) = Lit (sinh x)
  sinh _ = error "sinh not supported for this type"

  cosh (Lit x) = Lit (cosh x)
  cosh _ = error "cosh not supported for this type"

  tanh (Lit x) = Lit (tanh x)
  tanh _ = error "tanh not supported for this type"

  asinh (Lit x) = Lit (asinh x)
  asinh _ = error "asinh not supported for this type"

  acosh (Lit x) = Lit (acosh x)
  acosh _ = error "acosh not supported for this type"

  atanh (Lit x) = Lit (atanh x)
  atanh _ = error "atanh not supported for this type"

instance Integral (Expr b) where
  quot (Lit x) (Lit y) = Lit (quot x y)
  quot _ _ = error "quot not supported for this type"

  rem (Lit x) (Lit y) = Lit (rem x y)
  rem _ _ = error "rem not supported for this type"

  div (Lit x) (Lit y) = Lit (div x y)
  div _ _ = error "div not supported for this type"

  mod (Lit x) (Lit y) = Lit (mod x y)
  mod _ _ = error "mod not supported for this type"

  quotRem _ _ = error "quotRem not supported" 
  divMod _ _ = error "divMod not supported"

  toInteger (Lit x) = toInteger x
  toInteger _ = error "toInteger not supported for this type"
 
instance Real (Expr b) where
  toRational (Lit x) = toRational x
  toRational _ = error "toRational not supported for this type"

instance RealFrac (Expr b) where
  properFraction = error "properFraction not supported"
  
  truncate (Lit x) = truncate x
  truncate _ = error "truncate is not supported for this type"

  round (Lit x) = round x
  round _ = error "round is not supported for this type"

  ceiling (Lit x) = ceiling x
  ceiling _ = error "ceiling is not supported for this type"

  floor (Lit x) = floor x
  floor _ = error "floor is not supported for this type"

instance RealFloat (Expr b) where
  floatRadix (Lit x) = floatRadix x
  floatDigits (Lit x) = floatDigits x
  floatRange _ = error "float range is not supported for this type"
  decodeFloat _ = error "decode float is not supported for this type"
  encodeFloat x y = Lit (encodeFloat x y)
  exponent (Lit x) = exponent x
  significand (Lit x) = Lit (significand x)
  scaleFloat x (Lit y) = Lit (scaleFloat x y)
  isNaN (Lit x) = isNaN x
  isInfinite (Lit x) = isInfinite x
  isDenormalized (Lit x) = isDenormalized x
  isNegativeZero (Lit x) = isNegativeZero x
  isIEEE (Lit x) = isIEEE x
  atan2 (Lit x) (Lit y) = Lit (atan2 x y)  

instance Num Literal where
  (+) (LitNumber _ x) (LitNumber _ y) = integerToCoreLiteral ((Prelude.+) x y)
  (+) (LitDouble x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromRational y))
  (+) (LitNumber _ x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.+) (fromInteger x) (fromRational y))
  (+) (LitDouble x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromInteger y))
  (+) (LitFloat x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromRational y))
  (+) (LitNumber _ x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.+) (fromInteger x) (fromRational y))
  (+) (LitFloat x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromInteger y))
  (+) (LitFloat x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromRational y))
  (+) (LitDouble x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromRational y))
  (+) _ _ = error "+ not supported by this type"
  (-) (LitNumber _ x) (LitNumber _ y) = integerToCoreLiteral ((Prelude.-) x y)
  (-) (LitDouble x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromRational y))
  (-) (LitNumber _ x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.-) (fromInteger x) (fromRational y))
  (-) (LitDouble x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromInteger y))
  (-) (LitFloat x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromRational y))
  (-) (LitNumber _ x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.-) (fromInteger x) (fromRational y))
  (-) (LitFloat x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromInteger y))
  (-) (LitFloat x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromRational y))
  (-) (LitDouble x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromRational y))
  (-) _ _ = error "- not supported by this type"
  (*) (LitNumber _ x) (LitNumber _ y) = integerToCoreLiteral ((Prelude.*) x y)
  (*) (LitDouble x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromRational y))
  (*) (LitNumber _ x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.*) (fromInteger x) (fromRational y))
  (*) (LitDouble x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromInteger y))
  (*) (LitFloat x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromRational y))
  (*) (LitNumber _ x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.*) (fromInteger x) (fromRational y))
  (*) (LitFloat x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromInteger y))
  (*) (LitFloat x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromRational y))
  (*) (LitDouble x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromRational y))
  (*) _ _ = error "* not supported by this type"
  signum (LitNumber _ x) = integerToCoreLiteral (signum (fromInteger x))
  signum (LitDouble x) = fractionalToCoreLiteral (signum x)
  signum (LitFloat x) = fractionalToCoreLiteral (signum x)
  signum _ = error "signum not supported for this type"
  fromInteger x = integerToCoreLiteral x
  abs (LitNumber _ x) = integerToCoreLiteral (abs x)
  abs (LitDouble x) = fractionalToCoreLiteral (abs (fromRational x))
  abs (LitFloat x) = fractionalToCoreLiteral (abs (fromRational x))
  abs _ = error "abs not supported for this type"

instance Fractional Literal where
  (/) (LitNumber _ x) (LitNumber _ y) = LitDouble ((Prelude./) (fromInteger x) (fromInteger y))
  (/) (LitDouble x) (LitDouble y) = fractionalToCoreLiteral ((Prelude./) x y)
  (/) (LitNumber _ x) (LitDouble y) = fractionalToCoreLiteral ((Prelude./) (fromInteger x) y)
  (/) (LitDouble x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude./) x (fromInteger y))
  (/) (LitFloat x) (LitFloat y) = fractionalToCoreLiteral ((Prelude./) x y)
  (/) (LitNumber _ x) (LitFloat y) = fractionalToCoreLiteral ((Prelude./) (fromInteger x) y)
  (/) (LitFloat x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude./) x (fromInteger y))
  (/) (LitFloat x) (LitDouble y) = fractionalToCoreLiteral ((Prelude./) x y)
  (/) (LitDouble x) (LitFloat y) = fractionalToCoreLiteral ((Prelude./) x y)
  (/) _ _ = error "/ not supported by this type"
  recip expression = 1 / expression
  fromRational x = (LitDouble x)

instance Enum Literal where
  succ (LitNumber _ x) =  integerToCoreLiteral (succ x)
  succ (LitDouble x) =  fractionalToCoreLiteral (succ x)
  succ (LitFloat x) =  fractionalToCoreLiteral (succ x)
  succ (LitChar x) =  charToCoreLiteral (succ x)
  succ _ = error "succ not supported for this type"

  fromEnum (LitNumber _ x) = fromEnum x
  fromEnum (LitDouble x) = fromEnum x
  fromEnum (LitFloat x) = fromEnum x
  fromEnum (LitChar x) = fromEnum x
  fromEnum _ = error "fromEnum not supported for this type"

  enumFrom _ = error "enumFrom not supported" --can be implemented when lists can be created
  enumFromThen _ = error "enumFromThen not supported" --can be implemented when lists can be created
  enumFromTo _ = error "enumFromTo not supported" --can be implemented when lists can be created
  enumFromThenTo _ = error "enumFromThenTo not supported" --can be implemented when lists can be created
  
  toEnum =  integerToCoreLiteral.toInteger

instance Floating Literal where
  pi = fractionalToCoreLiteral Prelude.pi
  exp (LitDouble x) = fractionalToCoreLiteral (Prelude.exp (fromRational x ))
  exp (LitFloat x) = fractionalToCoreLiteral (Prelude.exp (fromRational x ))
  exp _ = error "exp not supported for this type"
  
  log (LitDouble x) = fractionalToCoreLiteral (Prelude.log (fromRational x ))
  log (LitFloat x) = fractionalToCoreLiteral (Prelude.log (fromRational x ))
  log _ = error "log not supported for this type"
  
  sqrt (LitDouble x) = fractionalToCoreLiteral (Prelude.sqrt (fromRational x ))
  sqrt (LitFloat x) = fractionalToCoreLiteral (Prelude.sqrt (fromRational x ))
  sqrt _ = error "sqrt not supported for this type"
 
  sin (LitDouble x) = fractionalToCoreLiteral (Prelude.sin (fromRational x ))
  sin (LitFloat x) = fractionalToCoreLiteral (Prelude.sin (fromRational x ))
  sin _ = error "sin not supported for this type"
  
  cos (LitDouble x) = fractionalToCoreLiteral (Prelude.cos (fromRational x ))
  cos (LitFloat x) = fractionalToCoreLiteral (Prelude.cos (fromRational x ))
  cos _ = error "cos not supported for this type"
  
  tan (LitDouble x) = fractionalToCoreLiteral (Prelude.tan (fromRational x ))
  tan (LitFloat x) = fractionalToCoreLiteral (Prelude.tan (fromRational x ))
  tan _ = error "tan not supported for this type"
  
  asin (LitDouble x) = fractionalToCoreLiteral (Prelude.asin (fromRational x ))
  asin (LitFloat x) = fractionalToCoreLiteral (Prelude.asin (fromRational x ))
  asin _ = error "asin not supported for this type"
  
  acos (LitDouble x) = fractionalToCoreLiteral (Prelude.acos (fromRational x ))
  acos (LitFloat x) = fractionalToCoreLiteral (Prelude.acos (fromRational x ))
  acos _ = error "acos not supported for this type"
  
  atan (LitDouble x) = fractionalToCoreLiteral (Prelude.atan (fromRational x ))
  atan (LitFloat x) = fractionalToCoreLiteral (Prelude.atan (fromRational x ))
  atan _ = error "atan not supported for this type"
  
  sinh (LitDouble x) = fractionalToCoreLiteral (Prelude.sinh (fromRational x ))
  sinh (LitFloat x) = fractionalToCoreLiteral (Prelude.sinh (fromRational x ))
  sinh _ = error "sinh not supported for this type"
  
  cosh (LitDouble x) = fractionalToCoreLiteral (Prelude.cosh (fromRational x ))
  cosh (LitFloat x) = fractionalToCoreLiteral (Prelude.cosh (fromRational x ))
  cosh _ = error "cosh not supported for this type"

  tanh (LitDouble x) = fractionalToCoreLiteral (Prelude.tanh (fromRational x ))
  tanh (LitFloat x) = fractionalToCoreLiteral (Prelude.tanh (fromRational x ))
  tanh _ = error "tanh not supported for this type"

  asinh (LitDouble x) = fractionalToCoreLiteral (Prelude.asinh (fromRational x ))
  asinh (LitFloat x) = fractionalToCoreLiteral (Prelude.asinh (fromRational x ))
  asinh _ = error "asinh not supported for this type"

  acosh (LitDouble x) = fractionalToCoreLiteral (Prelude.acosh (fromRational x ))
  acosh (LitFloat x) = fractionalToCoreLiteral (Prelude.acosh (fromRational x ))
  acosh _ = error "acosh not supported for this type"

  atanh (LitDouble x) = fractionalToCoreLiteral (Prelude.atanh (fromRational x ))
  atanh (LitFloat x) = fractionalToCoreLiteral (Prelude.atanh (fromRational x ))
  atanh _ = error "atanh not supported for this type"

  x ** y = exp (log x * y)
  logBase x y = log y / log x


instance Real Literal where
  toRational (LitNumber _ x) = fromInteger x
  toRational (LitDouble x) = x
  toRational (LitFloat x) = x
  toRational _ = error "toRational not supported for this type"

instance Integral Literal where
  quot (LitNumber _ x) (LitNumber _ y) = integerToCoreLiteral (quot x y)
  quot _ _ = error "quot not supported for this type"
  rem (LitNumber _ x) (LitNumber _ y) = integerToCoreLiteral (rem x y)
  rem _ _ = error "rem not supported for this type"
  div (LitNumber _ x) (LitNumber _ y) = integerToCoreLiteral (div x y)  
  div _ _ = error "div not supported for this type"
  mod (LitNumber _ x) (LitNumber _ y) = integerToCoreLiteral (mod x y) 
  mod _ _ = error "mod not supported for this type"
  quotRem _ _ = error "quotRem not supported" --can be implemented when tuples can be created
  divMod _ _ = error "divMod not supported" --can be implemented when tuples can be created
  toInteger (LitNumber _ x) = x
  toInteger _ = error "toInteger not supported for this type"

instance RealFrac Literal where
  properFraction _ = error "properFraction not supported"
  truncate (LitDouble x) = truncate x
  truncate (LitFloat x) = truncate x
  truncate _ = error "truncate not supported for this type"
  round (LitDouble x) = round x
  round (LitFloat x) = round x
  round _ = error "round not supported for this type"
  ceiling (LitDouble x) = ceiling x
  ceiling (LitFloat x) = ceiling x
  ceiling _ = error "ceiling not supported for this type"
  floor (LitDouble x) = floor x
  floor (LitFloat x) = floor x
  floor _ = error "floor not supported for this type"

instance RealFloat Literal where
  floatRadix (LitDouble x) = floatRadix (fromRational x)
  floatRadix (LitFloat x) = floatRadix (fromRational x)
  floatRadix _ = error "floatRadix not supported for this type"
  
  floatDigits (LitDouble x) = floatDigits (fromRational x)
  floatDigits (LitFloat x) = floatDigits (fromRational x)  
  floatDigits _ = error "floatDigits not supported for this type"
  
  exponent (LitDouble x) = exponent (fromRational x)
  exponent (LitFloat x) = exponent (fromRational x)
  exponent _ = error "exponent not supported for this type"
  
  
  significand (LitDouble x) = significand (fromRational x)
  significand (LitFloat x) = significand (fromRational x)
  significand _ = error "significand not supported for this type"

  scaleFloat  x (LitDouble y) = (scaleFloat x (fromRational y))
  scaleFloat  x (LitFloat y) = (scaleFloat x (fromRational y))
  scaleFloat _ _ = error "scaleFloat not supported for this type"

  isNaN (LitDouble x) = isNaN (fromRational x)
  isNaN (LitFloat x) = isNaN (fromRational x)
  isNaN _ = error "isNaN not supported for this type"

  isInfinite (LitDouble x) = isInfinite (fromRational x)
  isInfinite (LitFloat x) = isInfinite (fromRational x)
  isInfinite _ = error "isInfinite not supported for this type"

  isDenormalized (LitDouble x) = isDenormalized (fromRational x)
  isDenormalized (LitFloat x) = isDenormalized (fromRational x)
  isDenormalized _ = error "isDenormalized not supported for this type"

  isNegativeZero (LitDouble x) = isNegativeZero (fromRational x)
  isNegativeZero (LitFloat x) = isNegativeZero (fromRational x)
  isNegativeZero _ = error "isNegativeZero not supported for this type"

  isIEEE (LitDouble x) = isIEEE (fromRational x)
  isIEEE (LitFloat x) = isIEEE (fromRational x)
  isIEEE _ = error "isIEEE not supported for this type"

  atan2 (LitDouble x) (LitDouble y) = (atan2 (fromRational x) (fromRational y))
  atan2 (LitFloat x) (LitFloat y) = (atan2 (fromRational x) (fromRational y))
  atan2 _ _ = error "atan2 not supported for this type"

  encodeFloat x y = (encodeFloat x y)

  floatRange = error "floatRange not supported" --can be implemented when tuples can be created
  
  decodeFloat = error "decodeFloat not supported" --can be implemented when tuples can be created