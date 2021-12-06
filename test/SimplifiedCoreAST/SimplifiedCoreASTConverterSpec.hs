module SimplifiedCoreAST.SimplifiedCoreASTConverterSpec where
    
import Test.Hspec
import Test.Hspec.QuickCheck

import GHC.Types.Literal

import SimplifiedCoreAST.SimplifiedCoreASTConverter
import SimplifiedCoreAST.SimplifiedCoreAST

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

import GHC.Driver.Types (ModGuts (mg_binds))
import GHC.Paths (libdir)





spec :: Spec
spec = do
    describe "isTypeInformation" $ do
        it "can identifie type" $ do
            isTypeInformation TypeS `shouldBe` True

        it "defaults to false" $ do
            isTypeInformation 3 `shouldBe` False
        
    --describe "simplifyLiteral" $ do
        --it "can simplify float literals" $ do
            --  let expected = LitFloatS 3.5
            --let input = LitFloat 3.5
            -- simplifyLiteral input `shouldBe` expected
        --it "can simplify char literals" $ do
            -- let expected = LitCharS 'c'
            -- let input = LitFloat 3.5
            -- simplifyLiteral input `shouldBe` expected

                