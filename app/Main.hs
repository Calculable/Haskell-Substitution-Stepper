{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.State.Strict
  ( MonadIO (..),
    MonadState (get),
    StateT (runStateT),
    modify,
  )
import GHC
  ( DesugaredModule (dm_core_module),
    DynFlags (hscTarget),
    HscTarget (HscNothing),
    LoadHowMuch (LoadAllTargets),
    ParsedModule (pm_parsed_source),
    SuccessFlag (Failed, Succeeded),
    TypecheckedModule (tm_typechecked_source),
    addTarget,
    desugarModule,
    getModSummary,
    getSessionDynFlags,
    guessTarget,
    load,
    mkModuleName,
    parseModule,
    runGhc,
    setSessionDynFlags,
    typecheckModule,
  )
import GHC.Core (Bind (NonRec), Expr (..))
import GHC.Core.Ppr
  ( pprCoreAlt,
    pprCoreBinding,
    pprOptCo,
    pprParendExpr,
    pprCoreBindings
  )
import GHC.Driver.Types (ModGuts (mg_binds))
import GHC.Paths (libdir)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString),
  )
import GHC.Types.Var (Var (varName, varType))
import GHC.Utils.Outputable (Outputable (ppr), OutputableBndr)
import TypedStepperProofOfConceptExamples (printExampleStepping)
import Utils (printAst, showOutputable)
import FlatCoreASTPrinter (printFlatCoreAST)
import SimplifiedCoreAST.SimplifiedCoreAST (ExpressionS(..), LiteralS(..), AltS(..), AltConS(..), BindS(..))
import SimplifiedCoreAST.SimplifiedCoreASTConverter (simplifyBindings)
import SimplifiedCoreAST.SimplifiedCoreASTPrinter (printSimplifiedCoreAST)

main :: IO ((), StepState)
main = runGhc (Just libdir) $ do
  dFlags <- getSessionDynFlags
  setSessionDynFlags dFlags {hscTarget = HscNothing}

  target <- guessTarget "src/Source2.hs" Nothing
  addTarget target
  res <- load LoadAllTargets
  case res of
    Succeeded -> liftIO $ putStrLn "successfully loaded targets"
    Failed -> liftIO $ putStrLn "failed to load targets"

  let modName = mkModuleName "Source2"
  modSum <- getModSummary modName

  psmod <- parseModule modSum
  tcmod <- typecheckModule psmod
  dsmod <- desugarModule tcmod

  let parserAst = pm_parsed_source psmod
      tcAst = tm_typechecked_source tcmod

      coreModule = dm_core_module dsmod
      coreAst = mg_binds coreModule
  -- coreReaderEnv = mg_rdr_env coreModule
  -- coreTyCons = mg_tcs coreModule
  -- coreClassInsts = mg_insts coreModule
  -- coreFamInsts = mg_fam_insts coreModule
  -- corePatternSyns = mg_patsyns coreModule

  liftIO $ writeFile "parserAst.txt" (printAst parserAst)
  liftIO $ writeFile "tcAst.txt" (printAst tcAst)
  liftIO $ writeFile "coreAST.txt" (printAst coreAst)
  liftIO $ writeFile "coreProgram.txt" (showOutputable coreAst)
  -- liftIO $ writeFile "coreReaderEnv.txt" (printAst coreReaderEnv)
  -- liftIO $ writeFile "coreTyCons.txt" (printAst coreTyCons)
  -- liftIO $ writeFile "coreClassInsts.txt" (printAst coreClassInsts)
  -- liftIO $ writeFile "coreFamInsts.txt" (showOutputable coreFamInsts)
  -- liftIO $ writeFile "corePatternSyns.txt" (printAst corePatternSyns)

  liftIO $ putStrLn "\n*****Pretty Printed Core Bindings:******"
  liftIO (putStrLn (showOutputable (pprCoreBindings coreAst)))

  --liftIO $ putStrLn "\n*****Proof of concept******"
  --liftIO printExampleStepping

  let addAst = extract coreAst

  --liftIO $ putStrLn "\n*****Example Flat Printing of Source.hs:*****"
  --liftIO $ printFlatCoreAST coreAst

  liftIO $ putStrLn "\n*****Example Simplified Core AST Printing of Source.hs:*****"
  liftIO $ printSimplifiedCoreAST (simplifyBindings coreAst)

  liftIO $ putStrLn "\n*****Example Stepping of Source.hs:*****"
  runStateT (step addAst) initStepState

extract :: [Bind a] -> Expr a
extract prog =
  let (main : add : rest) = prog
   in case add of
        NonRec j exp -> exp

step :: (OutputableBndr b, MonadState StepState m, MonadIO m) => Expr b -> m ()
step (Var id) = do
  printDepth
  lPrint ("Var", showOutputable $ varName id, showOutputable $ varType id)
step (Lit lit) = do
    printDepth
    case lit of
      LitChar c -> lPrint ("Char ", c)
      LitNumber t v -> lPrint ("Number ", v)
      LitString bs -> lPrint ("String ", bs)
      LitFloat f -> lPrint ("Float ", f)
      LitDouble d -> lPrint ("Double ", d)
step (App exp arg) = do
  printDepth
  lPutStr "App "
  lOutput $ pprParendExpr exp
  incDepth
  step exp
  step arg
step (Lam b exp) = do
  printDepth
  lPutStr "Lam "
  lOutput' b
  lOutput $ pprParendExpr exp
  incDepth
  step exp
step (Let bind exp) = do
  printDepth
  lPutStr "Let "
  lOutput $ pprCoreBinding bind
  lOutput $ pprParendExpr exp
  incDepth
  step exp
step (Case exp b t alts) = do
  printDepth
  lPutStr "Case "
  lOutput $ pprParendExpr exp
  lOutput' b
  lOutput' $ ppr t
  lOutput $ map pprCoreAlt alts
  incDepth
  step exp
step (Cast exp coer) = do
  printDepth
  lPutStr "Cast "
  lOutput $ pprParendExpr exp
  lOutput $ pprOptCo coer
  incDepth
  step exp
step (Tick tid exp) = do
  printDepth
  lPutStr "Tick "
  lOutput $ pprParendExpr exp
  incDepth
  step exp
step (Type t) = do
  printDepth
  lPutStr "Type "
  lOutput $ ppr t
step (Coercion coer) = do
  printDepth
  lPutStr "Coer "
  lOutput $ pprOptCo coer

-- lifted version of common functions used in the stepper
lOutput :: (MonadState StepState m, MonadIO m, Outputable a) => a -> m ()
lOutput = liftIO . putStrLn . showOutputable

lOutput' :: (MonadState StepState m, MonadIO m, Outputable a) => a -> m ()
lOutput' = liftIO . putStr . showOutputable

lPrint :: (MonadState StepState m, MonadIO m, Show a) => a -> m ()
lPrint = liftIO . print

lPutStr :: (MonadState StepState m, MonadIO m) => String -> m ()
lPutStr = liftIO . putStr

-- state of the stepper
data StepState = StepState {depth :: Integer} deriving (Show)

initStepState :: StepState
initStepState = StepState 0

incDepth :: (MonadState StepState m, MonadIO m) => m ()
incDepth = modify $ \s -> StepState {depth = depth s + 1}

printDepth :: (MonadState StepState m, MonadIO m) => m ()
printDepth = do
  s <- get
  lPutStr (replicate (fromIntegral (depth s)) '=')
