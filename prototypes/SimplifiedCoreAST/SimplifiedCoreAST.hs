module SimplifiedCoreAST.SimplifiedCoreAST
  ( ExpressionS (..),
    LiteralS (..),
    AltS (..),
    AltConS (..),
    BindS (..),
  )
where

type BindS = (String, ExpressionS)

data ExpressionS
  = VarS String --for example "x" or "+"
  | LitS LiteralS --for example "4"
  | AppS {expressionS :: ExpressionS, argumentS :: ExpressionS} --for example "+ 1"
  | LamS {parameterS :: String, expressionS :: ExpressionS} --for example "\x -> ...""
  | CaseS {expressionS :: ExpressionS, alternativesS :: [AltS]} --for example "\a -> case (== a 1) of {true -> "One" false -> "not one"};"
  | TypeS --not implemented
  | MultiArgumentAppS {name :: String, argumentsS :: [ExpressionS]} --not in original core but used so we can make a reduction with build-in functions/operators from the prelude like (+ 1) 2 -> (+ 1 2) -> 3
  | InvalidExpression String --for example "unsupported expression"

data LiteralS
  = LitCharS Char
  | LitNumberS Integer
  | LitStringS String
  | LitFloatS Rational
  | LitDoubleS Rational
  | InvalidLiteral String

type AltS = (AltConS, [String], ExpressionS)

data AltConS
  = DataAltS String --pattern is a constructor, for example ":"
  | LitAltS LiteralS -- pattern is a literal, for example 5
  | DefaultS -- pattern is "_"

{-Type Classes-}
instance Num ExpressionS where
  (+) (LitS x) (LitS y) = LitS ((+) x y)
  (+) _ _ = InvalidExpression "+ not supported by this type"
  (-) (LitS x) (LitS y) = LitS ((-) x y)
  (-) _ _ = InvalidExpression "- not supported by this type"
  (*) (LitS x) (LitS y) = LitS ((*) x y)
  (*) _ _ = InvalidExpression "* not supported by this type"
  signum (LitS x) = LitS (signum x)
  signum _ = InvalidExpression "signum not supported by this type"
  fromInteger x = LitS (fromInteger x)
  abs (LitS x) = LitS (abs x)
  abs _ = InvalidExpression "abs not supported by this type"

{-Type Classes-}
instance Num LiteralS where
  (+) (LitNumberS x) (LitNumberS y) = LitNumberS ((Prelude.+) x y)
  (+) (LitDoubleS x) (LitDoubleS y) = LitDoubleS ((Prelude.+) x y)
  (+) (LitNumberS x) (LitDoubleS y) = LitDoubleS ((Prelude.+) (fromInteger x) y)
  (+) (LitDoubleS x) (LitNumberS y) = LitDoubleS ((Prelude.+) x (fromInteger y))
  (+) _ _ = InvalidLiteral "+ not supported by this type"
  (-) (LitNumberS x) (LitNumberS y) = LitNumberS ((Prelude.-) x y)
  (-) (LitDoubleS x) (LitDoubleS y) = LitDoubleS ((Prelude.-) x y)
  (-) (LitNumberS x) (LitDoubleS y) = LitDoubleS ((Prelude.-) (fromInteger x) y)
  (-) (LitDoubleS x) (LitNumberS y) = LitDoubleS ((Prelude.-) x (fromInteger y))
  (-) _ _ = InvalidLiteral "- not supported by this type"
  (*) (LitNumberS x) (LitNumberS y) = LitNumberS ((Prelude.*) x y)
  (*) (LitDoubleS x) (LitDoubleS y) = LitDoubleS ((Prelude.*) x y)
  (*) (LitNumberS x) (LitDoubleS y) = LitDoubleS ((Prelude.*) (fromInteger x) y)
  (*) (LitDoubleS x) (LitNumberS y) = LitDoubleS ((Prelude.*) x (fromInteger y))
  (*) _ _ = InvalidLiteral "* not supported by this type"
  signum (LitNumberS x) = LitDoubleS (signum (fromInteger x))
  signum (LitDoubleS x) = LitDoubleS (signum x)
  signum _ = InvalidLiteral "signum not supported for this type"
  fromInteger x = LitNumberS x
  abs (LitNumberS x) = LitNumberS (abs x)
  abs (LitDoubleS x) = LitDoubleS (abs x)
  abs _ = InvalidLiteral "abs not supported for this type"

instance Fractional ExpressionS where
  (/) (LitS x) (LitS y) = LitS ((/) x y)
  (/) _ _ = InvalidExpression "/ not supported by this type"
  recip (LitS x) = LitS (recip x)
  recip _ = InvalidExpression "recip not supported by this type"
  fromRational x = LitS (fromRational x)

instance Fractional LiteralS where
  (/) (LitNumberS x) (LitNumberS y) = LitDoubleS ((Prelude./) (fromInteger x) (fromInteger y))
  (/) (LitDoubleS x) (LitDoubleS y) = LitDoubleS ((Prelude./) x y)
  (/) (LitNumberS x) (LitDoubleS y) = LitDoubleS ((Prelude./) (fromInteger x) y)
  (/) (LitDoubleS x) (LitNumberS y) = LitDoubleS ((Prelude./) x (fromInteger y))
  (/) _ _ = InvalidLiteral "/ not supported by this type"
  recip expression = 1 / expression
  fromRational x = LitDoubleS x

instance Eq ExpressionS where
  (/=) (LitS x) (LitS y) = (/=) x y
  (/=) _ _ = False
  (==) (LitS x) (LitS y) = (==) x y
  (==) _ _ = False

instance Eq LiteralS where
  (/=) leftExpression rightExpression = not (leftExpression == rightExpression)
  (==) (LitNumberS x) (LitNumberS y) = (Prelude.==) x y
  (==) (LitDoubleS x) (LitDoubleS y) = (Prelude.==) x y
  (==) (LitNumberS x) (LitDoubleS y) = (Prelude.==) (fromInteger x) y
  (==) (LitDoubleS x) (LitNumberS y) = (Prelude.==) x (fromInteger y)
  (==) (LitStringS x) (LitStringS y) = (Prelude.==) x y
  (==) (LitCharS x) (LitCharS y) = (Prelude.==) x y
  (==) x y = False

instance Ord ExpressionS where
  (<=) (LitS x) (LitS y) = (<=) x y
  (<=) _ _ = False
  (<) (LitS x) (LitS y) = (<=) x y
  (<) _ _ = False
  (>=) (LitS x) (LitS y) = (<=) x y
  (>=) _ _ = False
  (>) (LitS x) (LitS y) = (<=) x y
  (>) _ _ = False

instance Ord LiteralS where
  compare leftExpression rightExpression
    | leftExpression == rightExpression = EQ
    | leftExpression <= rightExpression = LT
    | otherwise = GT

  (<=) (LitNumberS x) (LitNumberS y) = (Prelude.<=) x y
  (<=) (LitDoubleS x) (LitDoubleS y) = (Prelude.<=) x y
  (<=) (LitNumberS x) (LitDoubleS y) = (Prelude.<=) (fromInteger x) y
  (<=) (LitDoubleS x) (LitNumberS y) = (Prelude.<=) x (fromInteger y)
  (<=) (LitStringS x) (LitStringS y) = (Prelude.<=) x y
  (<=) (LitCharS x) (LitCharS y) = (Prelude.<=) x y
  (<=) x y = False
  (<) leftExpression rightExpression = compare leftExpression rightExpression == LT
  (>=) leftExpression rightExpression = compare leftExpression rightExpression /= LT
  (>) leftExpression rightExpression = compare leftExpression rightExpression == GT
