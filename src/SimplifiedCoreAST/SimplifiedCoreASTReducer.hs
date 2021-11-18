module SimplifiedCoreAST.SimplifiedCoreASTReducer
  (reduce
  )
where

import SimplifiedCoreAST.SimplifiedCoreAST (ExpressionS(..), LiteralS(..), AltS(..), AltConS(..), BindS(..))
import Data.List(isPrefixOf)
import SimplifiedCoreAST.SimplifiedCoreASTPrinter (printSimplifiedCoreExpression)

reduce :: [BindS] -> BindS -> IO ()
reduce context (x, expression) = do
    putStr "\n**Reduction of "
    putStr x
    putStr "**"
    putStrLn ""
    printSimplifiedCoreExpression expression 0
    putStrLn "\n{to be implemented}"
