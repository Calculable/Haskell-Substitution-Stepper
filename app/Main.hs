module Main where


import Control.Monad.Trans ( MonadIO(liftIO) )
import GHC hiding (pprParendExpr)
import GHC.Paths ( libdir )
import TypedStepperProofOfConceptExamples ( printExampleStepping )
import Utils ( dumpAST, showOutputable )
import GHC.Driver.Types (ModGuts(mg_binds, mg_rdr_env, mg_tcs, mg_fam_insts))
import GHC.Plugins (ModGuts(mg_insts, mg_patsyns), CoreProgram, CoreBind, Var)
import GHC.Core
import GHC.Types.Var
import GHC.Types.Literal
import GHC.Core.Ppr
import GHC.Utils.Outputable (OutputableBndr)
import qualified GHC.Core.Ppr

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
  dsmod <- desugarModule tcmod

  let parserAST = pm_parsed_source psmod
      tcAST = tm_typechecked_source tcmod

      coreModule = dm_core_module dsmod
      coreAst = mg_binds coreModule
      -- coreReaderEnv = mg_rdr_env coreModule
      -- coreTyCons = mg_tcs coreModule
      -- coreClassInsts = mg_insts coreModule
      -- coreFamInsts = mg_fam_insts coreModule
      -- corePatternSyns = mg_patsyns coreModule

  liftIO $ writeFile "parserAST.txt" (dumpAST parserAST)
  liftIO $ writeFile "tcAST.txt" (dumpAST tcAST)
  liftIO $ writeFile "coreAST.txt" (dumpAST coreAst)
  liftIO $ writeFile "coreProgram.txt" (showOutputable coreAst)
  -- liftIO $ writeFile "coreReaderEnv.txt" (dumpAST coreReaderEnv)
  -- liftIO $ writeFile "coreTyCons.txt" (dumpAST coreTyCons)
  -- liftIO $ writeFile "coreClassInsts.txt" (dumpAST coreClassInsts)
  -- liftIO $ writeFile "coreFamInsts.txt" (showOutputable coreFamInsts)
  -- liftIO $ writeFile "corePatternSyns.txt" (dumpAST corePatternSyns)

  liftIO printExampleStepping

  liftIO $ putStrLn "\nExample Stepping of Source:"
  liftIO $ step $ extract coreAst

extract :: [Bind a] -> Expr a
extract prog = let
  (main:add:rest) = prog
  in case add of
    NonRec j exp -> exp

step :: OutputableBndr b => Expr b -> IO ()
step (Var id) = print ("Var", showOutputable $ varName id, showOutputable $ varType id)
step (Lit lit) = case lit of
  LitChar c -> print ("Char ", c)
  LitNumber t v -> print ("Number ", v)
  LitString bs -> print ("String ", bs)
  LitFloat f -> print ("Float ", f)
  LitDouble d -> print ("Double ", d)
step (App exp arg) = do
  putStr "App "
  putStrLn . showOutputable $ pprParendExpr exp
  step exp
  step arg
step (Lam x exp) = do
  putStr "Lam "
  putStrLn . showOutputable $ pprParendExpr exp
  step exp
step (Let bind exp) = do
  putStr "Let "
  putStrLn . showOutputable $ pprParendExpr exp
  step exp
step (Case exp b t alts) = do
   putStr "Case "
   putStrLn . showOutputable $ pprParendExpr exp
   putStrLn . showOutputable $ map pprCoreAlt alts
   step exp
step (Cast exp coer) = do
   putStr "Cast "
   putStrLn . showOutputable $ pprParendExpr exp
   putStrLn . showOutputable $ pprOptCo coer
   step exp
step (Tick tid exp) = do
  putStr "Tick "
  putStrLn . showOutputable $ pprParendExpr exp
  step exp
step (Type t) = putStr "Type "
step (Coercion coer) = do
  putStr "Coer "
  putStrLn . showOutputable $ pprOptCo coer
