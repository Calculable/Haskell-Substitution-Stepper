module SimplifiedCoreAST.SimplifiedCoreASTPrinter
  (printSimplifiedCoreAST
  )
where

import SimplifiedCoreAST.SimplifiedCoreAST (ExpressionS(..), LiteralS(..), AltS(..), AltConS(..), BindS(..))
import Data.List(isPrefixOf)

printSimplifiedCoreAST :: [BindS] -> IO ()
printSimplifiedCoreAST x = mapM_ printSimplifiedCoreBinding x

printSimplifiedCoreBinding :: BindS -> IO ()
printSimplifiedCoreBinding (x, expression) = do
    putStr x
    putStr " = "
    printSimplifiedCoreExpression expression
    putStrLn ""

printSimplifiedCoreExpression :: ExpressionS -> IO ()
printSimplifiedCoreExpression (VarS var) = do
    if (isPrefixOf "$" var)
      then putStr ""
      else putStr var
printSimplifiedCoreExpression (LitS lit) = printLiteral lit
printSimplifiedCoreExpression (AppS exp arg) = do
  putStr "("
  printSimplifiedCoreExpression exp
  putStr " "
  printSimplifiedCoreExpression arg
  putStr ")"
printSimplifiedCoreExpression (LamS b exp) = do
  putStr "\\"
  putStr b
  putStr " -> ("
  printSimplifiedCoreExpression exp
  putStr ")"
printSimplifiedCoreExpression (TypeS) = putStr ""
printSimplifiedCoreExpression (CaseS exp alts) = do
  putStr "case "
  printSimplifiedCoreExpression exp
  putStr " of {"
  mapM_ printSimplifiedAlt alts
  putStr "};"
printSimplifiedCoreExpression (InvalidExpression reason) = do
  putStr ("Unsupported Expression " ++ reason)

printLiteral :: LiteralS -> IO ()
printLiteral (LitCharS c) = putStr (show c)
printLiteral (LitNumberS v) = putStr (show v)
printLiteral (LitStringS s) = putStr s
printLiteral (LitFloatS f) = putStr (show f)
printLiteral (LitDoubleS d) = putStr (show d)

printSimplifiedAlt :: AltS -> IO ()
printSimplifiedAlt (altCon, b, exp) = do
    putStr "\n\t"
    printSimplifiedAltCon altCon --either literal or constructor or _
    putStr (unwords b)
    putStr " -> "
    printSimplifiedCoreExpression exp

printSimplifiedAltCon :: AltConS -> IO ()
printSimplifiedAltCon (DataAltS x) = putStr x
printSimplifiedAltCon (LitAltS x) = printLiteral x
printSimplifiedAltCon (DefaultS) = putStr "_"
