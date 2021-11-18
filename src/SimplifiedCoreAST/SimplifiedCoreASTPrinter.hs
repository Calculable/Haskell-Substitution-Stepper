module SimplifiedCoreAST.SimplifiedCoreASTPrinter
  (printSimplifiedCoreAST, printSimplifiedCoreExpression
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
    printSimplifiedCoreExpression expression 0
    putStrLn ""

printSimplifiedCoreExpression :: ExpressionS -> Integer -> IO ()
printSimplifiedCoreExpression (VarS var) indent  = putStr var
printSimplifiedCoreExpression (LitS lit) indent = printLiteral lit indent
printSimplifiedCoreExpression (AppS exp arg) indent = do
  putStr "("
  printSimplifiedCoreExpression exp indent
  putStr " "
  printSimplifiedCoreExpression arg indent
  putStr ")"
printSimplifiedCoreExpression (LamS b exp) indent= do
  putStr "\\"
  putStr b
  putStr " -> ("
  printSimplifiedCoreExpression exp indent
  putStr ")"
printSimplifiedCoreExpression (TypeS) indent = putStr "(Type-Argument not supported)"
printSimplifiedCoreExpression (CaseS exp alts) indent = do
  putStr "case "
  printSimplifiedCoreExpression exp indent
  putStr " of {"
  mapM_ (printSimplifiedAlt (indent+1)) alts
  putStrLn("")
  printDepth indent
  putStr "}"
printSimplifiedCoreExpression (InvalidExpression reason) indent = do
  putStr ("Unsupported Expression " ++ reason)

printLiteral :: LiteralS -> Integer -> IO ()
printLiteral (LitCharS c) indent = putStr (show c)
printLiteral (LitNumberS v) indent = putStr (show v)
printLiteral (LitStringS s) indent = putStr s
printLiteral (LitFloatS f) indent = putStr (show f)
printLiteral (LitDoubleS d) indent = putStr (show d)

printSimplifiedAlt :: Integer -> AltS -> IO ()
printSimplifiedAlt indent (altCon, b, exp) = do
    putStrLn("")
    printDepth indent
    printSimplifiedAltCon indent altCon --either literal or constructor or _
    putStr (unwords b)
    putStr " -> "
    printSimplifiedCoreExpression exp indent
    putStr ";"

printSimplifiedAltCon :: Integer -> AltConS -> IO ()
printSimplifiedAltCon indent (DataAltS x) = putStr x
printSimplifiedAltCon indent (LitAltS x) = printLiteral x indent
printSimplifiedAltCon indent (DefaultS) = putStr "_"

printDepth :: Integer -> IO ()
printDepth depth = putStr (replicate (fromInteger depth) '\t')