{-# LANGUAGE FlexibleContexts #-}

module OldStepper where

import Control.Monad.State.Strict
    ( MonadIO(..), MonadState(get), modify )
import GHC ( isGlobalId, isLocalId )
import GHC.Core.Ppr
    ( pprCoreBinding, pprCoreAlt, pprOptCo, pprParendExpr )
import GHC.Plugins
    ( OutputableBndr,
      liftIO,
      Outputable(ppr),
      Expr(..),
      Literal(LitDouble, LitChar, LitNumber, LitString, LitFloat),
      isGlobalId,
      isId,
      isLocalId,
      isLocalVar,
      isTcTyVar,
      isTyVar,
      Var(varName, varType) )
import Utils ( showOutputable )

-- runStateT (step addAst) initStepState
step :: (OutputableBndr b, MonadState StepState m, MonadIO m) => Expr b -> m ()
step (Var id) = do
  printDepth
  lPrint ("Var", showOutputable $ varName id, showOutputable $ varType id, show (isId id), show (isTyVar id), show (isTcTyVar id), show (isLocalId id), show (isGlobalId id), show (isLocalVar id))
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