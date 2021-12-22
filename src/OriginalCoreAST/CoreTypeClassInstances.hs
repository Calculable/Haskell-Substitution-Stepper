{-|
Module      : CoreStepperPrinter
Description : Makes Core expressions and Core literals compatible with type classes from the prelude
License     : GPL-3

This module makes the type @ Expr @ and the type @ Literal @ compatible with type classes 
from the prelude: EQ, Fractional, Num, Ord, Enum, Float, Integral, Real, RealFrac
Other typeclasses are supported as well but their implementation is either integrated in the
steppable prelude or in the CoreEvaluator module.
-}
module OriginalCoreAST.CoreTypeClassInstances () where

import GHC.Plugins
import OriginalCoreAST.CoreInformationExtractorFunctions
import OriginalCoreAST.CoreMakerFunctions
import OriginalCoreAST.CoreStepperHelpers.CoreTracerHelper
import Utils
import Data.Maybe
import Data.Bifunctor
import Debug.Trace

instance OutputableBndr b => Show (Expr b) where
  show = showOutputable

instance Num (Expr b) where
  (+) (Lit x) (Lit y) = Lit ((+) x y)
  (+) _ _ = error "+ not supported by this type"
  (-) (Lit x) (Lit y) = Lit ((-) x y)
  (-) _ _ = error "- not supported by this type"
  (*) (Lit x) (Lit y) = Lit ((*) x y)
  (*) _ _ = error "* not supported by this type"
  signum (Lit x) = Lit (signum x)
  signum _ = error "signum not supported by this type"
  fromInteger = integerToCoreExpression
  abs (Lit x) = Lit (abs x)
  abs _ = error "abs not supported by this type"

instance Fractional (Expr b) where
  (/) (Lit x) (Lit y) = Lit ((/) x y)
  (/) _ _ = error "/ not supported by this type"
  recip (Lit x) = Lit (recip x)
  recip _ = error "recip not supported by this type"
  fromRational = rationalToCoreExpression

instance Eq (Expr b) where
  (/=) x y = not ((==) x y)
  (==) (Lit x) (Lit y) = weakEquals x y
  (==) (Var x) (Var y) = (==) (varToString x) (varToString y)
  (==) (App x1 y1) (App x2 y2) = trace "operator for collection called" operatorForCollection (App x1 y1) (App x2 y2) (==)
  (==) x y = error ("== and /= not supported by this type: " ++ typeOfExpression y)

instance Ord (Expr b) where
  (<=) (Lit x) (Lit y) = lessOrEqualLiteral x y
  (<=) (Var x) (Var y) | isBoolVar (Var x) && isBoolVar (Var y) = boolValueFromVar x <= boolValueFromVar y
  (<=) (App x1 y1) (App x2 y2) = operatorForCollection (App x1 y1) (App x2 y2) (<=)
  (<=) _ _ = error "<= not supported by this type"
  (<) (Lit x) (Lit y) = lessLiteral x y
  (<) (Var x) (Var y) | isBoolVar (Var x) && isBoolVar (Var y) = boolValueFromVar x < boolValueFromVar y
  (<) (App x1 y1) (App x2 y2) = operatorForCollection (App x1 y1) (App x2 y2) (<)
  (<) _ _ = error "< not supported by this type"
  (>=) (Lit x) (Lit y) = greaterEqualLiteral x y
  (>=) (Var x) (Var y) | isBoolVar (Var x) && isBoolVar (Var y) = boolValueFromVar x >= boolValueFromVar y
  (>=) (App x1 y1) (App x2 y2) = operatorForCollection (App x1 y1) (App x2 y2) (>=)
  (>=) _ _ = error ">= not supported by this type"
  (>) (Lit x) (Lit y) = greaterLiteral x y
  (>) (Var x) (Var y) | isBoolVar (Var x) && isBoolVar (Var y) = boolValueFromVar x > boolValueFromVar y
  (>) (App x1 y1) (App x2 y2) = operatorForCollection (App x1 y1) (App x2 y2) (>)
  (>) _ _ = error "> not supported by this type"    

instance Enum (Expr b) where
  succ (Lit x) = Lit (succ x)
  succ _ = error "succ not supported for this type"

  fromEnum (Lit x) = fromEnum x
  fromEnum _ = error "fromEnum not supported for this type"

  enumFrom (Lit x) = map Lit (enumFrom x)
  enumFrom _ = error "enumFrom not supported for this type"

  enumFromThen (Lit x) (Lit y) = map Lit (enumFromThen x y)
  enumFromThen _ _ = error "enumFromThen not supported for this type"

  enumFromTo (Lit x) (Lit y) = map Lit (enumFromTo x y)
  enumFromTo _ _ = error "enumFromTo not supported for this type"

  enumFromThenTo (Lit x) (Lit y) (Lit z) = map Lit (enumFromThenTo x y z)
  enumFromThenTo _ _ _ = error "enumFromThenTo not supported for this type"

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

  quotRem (Lit x) (Lit y) = Data.Bifunctor.bimap Lit Lit res where res = quotRem x y
  quotRem _ _ = error "quotRem not supported"

  divMod (Lit x) (Lit y) = Data.Bifunctor.bimap Lit Lit res where res = divMod x y
  divMod _ _ = error "divMod not supported"

  toInteger (Lit x) = toInteger x
  toInteger _ = error "toInteger not supported for this type"

instance Real (Expr b) where
  toRational (Lit x) = toRational x
  toRational _ = error "toRational not supported for this type"

instance RealFrac (Expr b) where
  properFraction (Lit x) = Data.Bifunctor.second Lit res where res = properFraction x
  properFraction _ = error "properFraction not supported for this type"

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
  floatRadix _ = error "floatRadix is not supported for this type"

  floatDigits (Lit x) = floatDigits x
  floatDigits _ = error "floatDigits is not supported for this type"

  floatRange (Lit x) = floatRange x
  floatRange _ = error "floatRange is not supported for this type"

  decodeFloat (Lit x) = decodeFloat x
  decodeFloat _ = error "decodeFloat is not supported for this type"

  encodeFloat x y = Lit (encodeFloat x y)

  exponent (Lit x) = exponent x
  exponent _ = error "exponent is not supported for this type"

  significand (Lit x) = Lit (significand x)
  significand _ = error "significand is not supported for this type"

  scaleFloat x (Lit y) = Lit (scaleFloat x y)
  scaleFloat _ _ = error "scaleFloat is not supported for this type"

  isNaN (Lit x) = isNaN x
  isNaN _ = error "isNaN is not supported for this type"

  isInfinite (Lit x) = isInfinite x
  isInfinite _ = error "isInfinite is not supported for this type"

  isDenormalized (Lit x) = isDenormalized x
  isDenormalized _ = error "isDenormalized is not supported for this type"

  isNegativeZero (Lit x) = isNegativeZero x
  isNegativeZero _ = error "isNegativeZero is not supported for this type"

  isIEEE (Lit x) = isIEEE x
  isIEEE _ = error "isIEEE is not supported for this type"

  atan2 (Lit x) (Lit y) = Lit (atan2 x y)
  atan2 _ _ = error "atan2 is not supported for this type"

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
  fromInteger = integerToCoreLiteral
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
  fromRational = LitDouble

instance Enum Literal where
  succ (LitNumber _ x) = integerToCoreLiteral (succ x)
  succ (LitDouble x) = fractionalToCoreLiteral (succ x)
  succ (LitFloat x) = fractionalToCoreLiteral (succ x)
  succ (LitChar x) = charToCoreLiteral (succ x)
  succ _ = error "succ not supported for this type"

  fromEnum (LitNumber _ x) = fromEnum x
  fromEnum (LitDouble x) = fromEnum x
  fromEnum (LitFloat x) = fromEnum x
  fromEnum (LitChar x) = fromEnum x
  fromEnum _ = error "fromEnum not supported for this type"

  enumFrom (LitNumber _ x) = map integerToCoreLiteral (enumFrom x)
  enumFrom (LitDouble x) = map rationalToCoreLiteral (enumFrom x)
  enumFrom (LitFloat x) = map rationalToCoreLiteral (enumFrom x)
  enumFrom (LitChar x) = map charToCoreLiteral (enumFrom x)
  enumFrom _ = error "enumFrom not supported for this type"

  enumFromThen (LitNumber _ x) (LitNumber _ y) = map integerToCoreLiteral (enumFromThen x y)
  enumFromThen (LitDouble x) (LitDouble y) = map rationalToCoreLiteral (enumFromThen x y)
  enumFromThen (LitFloat x) (LitFloat y) = map rationalToCoreLiteral (enumFromThen x y)
  enumFromThen (LitChar x) (LitChar y) = map charToCoreLiteral (enumFromThen x y)
  enumFromThen _ _ = error "enumFromThen not supported for this type"

  enumFromTo (LitNumber _ x) (LitNumber _ y) = map integerToCoreLiteral (enumFromTo x y)
  enumFromTo (LitDouble x) (LitDouble y) = map rationalToCoreLiteral (enumFromTo x y)
  enumFromTo (LitFloat x) (LitFloat y) = map rationalToCoreLiteral (enumFromTo x y)
  enumFromTo (LitChar x) (LitChar y) = map charToCoreLiteral (enumFromTo x y)
  enumFromTo _ _ = error "enumFromTo not supported for this type"

  enumFromThenTo (LitNumber _ x) (LitNumber _ y) (LitNumber _ z) = map integerToCoreLiteral (enumFromThenTo x y z)
  enumFromThenTo (LitDouble x) (LitDouble y) (LitDouble z) = map rationalToCoreLiteral (enumFromThenTo x y z)
  enumFromThenTo (LitFloat x) (LitFloat y) (LitFloat z) = map rationalToCoreLiteral (enumFromThenTo x y z)
  enumFromThenTo (LitChar x) (LitChar y) (LitChar z) = map charToCoreLiteral (enumFromThenTo x y z)
  enumFromThenTo _ _ _ = error "enumFromThenTo not supported for this type"

  toEnum = integerToCoreLiteral . toInteger

instance Floating Literal where
  pi = fractionalToCoreLiteral Prelude.pi
  exp (LitDouble x) = fractionalToCoreLiteral (Prelude.exp (fromRational x))
  exp (LitFloat x) = fractionalToCoreLiteral (Prelude.exp (fromRational x))
  exp _ = error "exp not supported for this type"

  log (LitDouble x) = fractionalToCoreLiteral (Prelude.log (fromRational x))
  log (LitFloat x) = fractionalToCoreLiteral (Prelude.log (fromRational x))
  log _ = error "log not supported for this type"

  sqrt (LitDouble x) = fractionalToCoreLiteral (Prelude.sqrt (fromRational x))
  sqrt (LitFloat x) = fractionalToCoreLiteral (Prelude.sqrt (fromRational x))
  sqrt _ = error "sqrt not supported for this type"

  sin (LitDouble x) = fractionalToCoreLiteral (Prelude.sin (fromRational x))
  sin (LitFloat x) = fractionalToCoreLiteral (Prelude.sin (fromRational x))
  sin _ = error "sin not supported for this type"

  cos (LitDouble x) = fractionalToCoreLiteral (Prelude.cos (fromRational x))
  cos (LitFloat x) = fractionalToCoreLiteral (Prelude.cos (fromRational x))
  cos _ = error "cos not supported for this type"

  tan (LitDouble x) = fractionalToCoreLiteral (Prelude.tan (fromRational x))
  tan (LitFloat x) = fractionalToCoreLiteral (Prelude.tan (fromRational x))
  tan _ = error "tan not supported for this type"

  asin (LitDouble x) = fractionalToCoreLiteral (Prelude.asin (fromRational x))
  asin (LitFloat x) = fractionalToCoreLiteral (Prelude.asin (fromRational x))
  asin _ = error "asin not supported for this type"

  acos (LitDouble x) = fractionalToCoreLiteral (Prelude.acos (fromRational x))
  acos (LitFloat x) = fractionalToCoreLiteral (Prelude.acos (fromRational x))
  acos _ = error "acos not supported for this type"

  atan (LitDouble x) = fractionalToCoreLiteral (Prelude.atan (fromRational x))
  atan (LitFloat x) = fractionalToCoreLiteral (Prelude.atan (fromRational x))
  atan _ = error "atan not supported for this type"

  sinh (LitDouble x) = fractionalToCoreLiteral (Prelude.sinh (fromRational x))
  sinh (LitFloat x) = fractionalToCoreLiteral (Prelude.sinh (fromRational x))
  sinh _ = error "sinh not supported for this type"

  cosh (LitDouble x) = fractionalToCoreLiteral (Prelude.cosh (fromRational x))
  cosh (LitFloat x) = fractionalToCoreLiteral (Prelude.cosh (fromRational x))
  cosh _ = error "cosh not supported for this type"

  tanh (LitDouble x) = fractionalToCoreLiteral (Prelude.tanh (fromRational x))
  tanh (LitFloat x) = fractionalToCoreLiteral (Prelude.tanh (fromRational x))
  tanh _ = error "tanh not supported for this type"

  asinh (LitDouble x) = fractionalToCoreLiteral (Prelude.asinh (fromRational x))
  asinh (LitFloat x) = fractionalToCoreLiteral (Prelude.asinh (fromRational x))
  asinh _ = error "asinh not supported for this type"

  acosh (LitDouble x) = fractionalToCoreLiteral (Prelude.acosh (fromRational x))
  acosh (LitFloat x) = fractionalToCoreLiteral (Prelude.acosh (fromRational x))
  acosh _ = error "acosh not supported for this type"

  atanh (LitDouble x) = fractionalToCoreLiteral (Prelude.atanh (fromRational x))
  atanh (LitFloat x) = fractionalToCoreLiteral (Prelude.atanh (fromRational x))
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
  quotRem (LitNumber _ x) (LitNumber _ y) = Data.Bifunctor.bimap integerToCoreLiteral integerToCoreLiteral res where res = quotRem x y
  quotRem _ _ = error "quotRem not supported for this type"
  divMod (LitNumber _ x) (LitNumber _ y) = Data.Bifunctor.bimap integerToCoreLiteral integerToCoreLiteral res where res = divMod x y
  divMod _ _ = error "divMod not supported for this type"
  toInteger (LitNumber _ x) = x
  toInteger _ = error "toInteger not supported for this type"

instance RealFrac Literal where
  properFraction (LitDouble x) = Data.Bifunctor.second rationalToCoreLiteral res where res = properFraction x
  properFraction (LitFloat y) = Data.Bifunctor.second rationalToCoreLiteral res where res = properFraction y
  properFraction _ = error "properFraction not supported for this type"
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

  scaleFloat x (LitDouble y) = scaleFloat x (fromRational y)
  scaleFloat x (LitFloat y) = scaleFloat x (fromRational y)
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

  atan2 (LitDouble x) (LitDouble y) = atan2 (fromRational x) (fromRational y)
  atan2 (LitFloat x) (LitFloat y) = atan2 (fromRational x) (fromRational y)
  atan2 _ _ = error "atan2 not supported for this type"

  encodeFloat = encodeFloat

  floatRange (LitDouble x) = floatRange (fromRational x)
  floatRange (LitFloat x) = floatRange (fromRational x)
  floatRange _ = error "floatRange not supported for this type"

  decodeFloat (LitDouble x) = decodeFloat (fromRational x)
  decodeFloat (LitFloat x) = decodeFloat (fromRational x)
  decodeFloat _ = error "decodeFloat not supported"

instance Show Literal where
  show (LitChar charValue) = show charValue
  show (LitNumber _ integerValue) = show integerValue
  show (LitString stringValue) = show stringValue
  show (LitFloat floatValue) = show floatValue
  show (LitDouble doubleValue) = show doubleValue
  show (LitNullAddr) = "NULL"
  show (LitRubbish) = "(LitRubbish)"
  show (LitLabel fastString _ _) = "(LitLabel)"

{-Helper functions-}

-- |takes two expression representing a list- or tuple-type and a boolean operator and applies the expression to the operator
operatorForCollection :: Expr b -> Expr b -> ([Expr b] -> [Expr b] -> Bool) -> Bool
operatorForCollection a b operator = operator (elementsToCompareForCollection a) (elementsToCompareForCollection b)
  where
    elementsToCompareForCollection :: Expr b -> [Expr b]
    elementsToCompareForCollection expr   | isList expr = removeTypeInformation (getIndividualElementsOfList expr)
                                          | isTuple expr = removeTypeInformation (getIndividualElementsOfTuple expr)
                                          | isPrimitiveTypeConstructorApp expr = [getLiteralArgument expr]
                                          | otherwise = error "operator not supported: unknown type: "

-- |compares two literal instances for equalty. 
-- Note that the Literal type implements the EQ typeclass. 
-- This comparison however is less strict (only the underlaying value is compared)
weakEquals :: Literal -> Literal -> Bool
weakEquals (LitChar first) (LitChar second) = (==) first second
weakEquals (LitNumber _ first) (LitNumber _ second) = (==) first second
weakEquals (LitString first) (LitString second) = (==) first second
weakEquals LitNullAddr LitNullAddr = True
weakEquals LitRubbish LitRubbish = True
weakEquals (LitFloat first) (LitFloat second) = (==) first second
weakEquals (LitDouble first) (LitFloat second) = (==) first second
weakEquals (LitFloat first) (LitDouble second) = (==) first second
weakEquals (LitDouble first) (LitDouble second) = (==) first second
weakEquals (LitLabel firstX firstY firstZ) (LitLabel secondX secondY secondZ) = (==) (LitLabel firstX firstY firstZ) (LitLabel secondX secondY secondZ)
weakEquals (LitNumber _ first) (LitFloat second) = (==) (fromInteger first) (fromRational second)
weakEquals (LitNumber _ first) (LitDouble second) = (==) (fromInteger first) (fromRational second)
weakEquals (LitFloat first) (LitNumber _ second) = (==) (fromRational first) (fromInteger second)
weakEquals (LitDouble first) (LitNumber _ second) = (==) (fromRational first) (fromInteger second)
weakEquals _ _ = False

-- |compares two literal instances. 
-- Note that the Literal type implements the ORD typeclass. 
-- This comparison however is less strict (only the underlaying value is compared)
compareLiteral :: Literal -> Literal -> Ordering
compareLiteral leftExpression rightExpression
  | weakEquals leftExpression rightExpression = EQ
  | lessOrEqualLiteral leftExpression rightExpression = LT
  | otherwise = GT

-- |checks if a literal is less or equal (<=) than the other literal.
-- Note that the Literal type implements the ORD typeclass. 
-- This comparison however is less strict (only the underlaying value is compared)
lessOrEqualLiteral :: Literal -> Literal -> Bool
lessOrEqualLiteral (LitChar x) (LitChar y) = x <= y
lessOrEqualLiteral (LitNumber _ x) (LitNumber _ y) = x <= y
lessOrEqualLiteral (LitString x) (LitString y) = x <= y
lessOrEqualLiteral (LitFloat x) (LitFloat y) = x <= y
lessOrEqualLiteral (LitDouble x) (LitDouble y) = x <= y
lessOrEqualLiteral (LitFloat x) (LitDouble y) = x <= y
lessOrEqualLiteral (LitDouble x) (LitFloat y) = x <= y
lessOrEqualLiteral (LitNumber _ x) (LitFloat y) = fromInteger x <= y
lessOrEqualLiteral (LitFloat x) (LitNumber _ y) = x <= fromInteger y
lessOrEqualLiteral (LitNumber _ x) (LitDouble y) = fromInteger x <= y
lessOrEqualLiteral (LitDouble x) (LitNumber _ y) = x <= fromInteger y
lessOrEqualLiteral x y = x <= y --use existing equality operator in literal type

-- |checks if a literal is less (<) than the other literal.
-- Note that the Literal type implements the ORD typeclass. 
-- This comparison however is less strict (only the underlaying value is compared)
lessLiteral :: Literal -> Literal -> Bool
lessLiteral leftExpression rightExpression = compareLiteral leftExpression rightExpression == LT

-- |checks if a literal is greater or equal (>=) than the other literal.
-- Note that the Literal type implements the ORD typeclass. 
-- This comparison however is less strict (only the underlaying value is compared)
greaterEqualLiteral :: Literal -> Literal -> Bool
greaterEqualLiteral leftExpression rightExpression = compareLiteral leftExpression rightExpression /= LT

-- |checks if a literal is greater (>) than the other literal.
-- Note that the Literal type implements the ORD typeclass. 
-- This comparison however is less strict (only the underlaying value is compared)
greaterLiteral :: Literal -> Literal -> Bool
greaterLiteral leftExpression rightExpression = compareLiteral leftExpression rightExpression == GT
