module Main where

import Cli (dispatch, runCli)

main :: IO ()
main = do
  invocation <- runCli
  dispatch invocation
