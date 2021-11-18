module SimplifiedCoreAST.SimplifiedCoreAST
  (ExpressionS(..), LiteralS(..), AltS(..), AltConS(..), BindS(..)
  )
where

type BindS = (String, ExpressionS)

data ExpressionS
    = VarS String --for example "x" or "+"
    | LitS LiteralS --for example "4"
    | AppS {expressionS:: ExpressionS, argumentS :: ExpressionS} --for example "+ 1"
    | LamS {parameterS:: String, expressionS:: ExpressionS} --for example "\x -> ...""
    | CaseS {expressionS:: ExpressionS, alternativesS:: [AltS]} --for example "\a -> case (== a 1) of {true -> "One" false -> "not one"};"
    | TypeS --not implemented
    | MultiArgumentAppS {name:: String, argumentsS :: [ExpressionS]} --not in original core but used so we can make a reduction with build-in functions/operators from the prelude like (+ 1) 2 -> (+ 1 2) -> 3 
    | InvalidExpression String --for example "unsupported expression"

data LiteralS
    = LitCharS Char
    | LitNumberS Integer
    | LitStringS String
    | LitFloatS Rational
    | LitDoubleS Rational

type AltS = (AltConS, [String], ExpressionS)

data AltConS
    = DataAltS String --pattern is a constructor, for example ":"
    | LitAltS  LiteralS -- pattern is a literal, for example "TRUE"
    | DefaultS -- pattern is "_"