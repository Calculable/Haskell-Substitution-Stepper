module OriginalCoreAST.OriginalCoreASTReducerSpec where
    
import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Maybe
import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt, AltCon (..), CoreBind, collectArgs)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString), mkLitInt64, mkLitString
  )
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)

import OriginalCoreAST.OriginalCoreASTReducer

spec :: spec
spec = do
    describe "applyUnsteppableFunctionToArguments" $ do
        context "when used with unimplemented function" $ do
            prop "return Nothing" $
                \x -> applyUnsteppableFunctionToArguments x `shouldBe` (Nothing::Maybe(Expr Var))