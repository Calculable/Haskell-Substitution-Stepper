module Main where
import GHC
import GHC.Paths
import Control.Monad.Trans
import Utils

main :: IO ()
main = runGhc (Just libdir) $ do
  dFlags <- getSessionDynFlags
  setSessionDynFlags dFlags {hscTarget = HscNothing}

  target <- guessTarget "src/Source.hs" Nothing
  addTarget target
  res <- load LoadAllTargets
  case res of
    Succeeded -> liftIO $ putStrLn "successfully loaded targets"
    Failed -> liftIO $ putStrLn "failed to load targets"

  let modName = mkModuleName "Source"
  modSum <- getModSummary modName

  psmod <- parseModule modSum
  tcmod <- typecheckModule psmod

  let parserAST = pm_parsed_source psmod
      tcAST = tm_typechecked_source tcmod

  liftIO $ writeFile "parserAST.txt" (dumpAST parserAST)
  liftIO $ writeFile "tcAST.txt" (dumpAST tcAST)
