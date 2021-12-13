module Main where

import Cli (dispatch, runCli)

main :: IO ()
main = do
  invocation <- runCli
  print invocation
  dispatch invocation
