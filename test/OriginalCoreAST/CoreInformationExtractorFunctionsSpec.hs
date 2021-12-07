module OriginalCoreAST.CoreInformationExtractorFunctionsSpec where
    
import Test.Hspec
import Test.Hspec.QuickCheck
import DataProvider.DataProvider(coreBindings, findBinding, bindingFinder)

import OriginalCoreAST.CoreInformationExtractorFunctions()
import Utils (showOutputable)
import GHC.Plugins (liftIO )
import GHC.Core (Bind (NonRec), CoreProgram, Expr)
import GHC.Types.Var (Var (varName, varType))
import OriginalCoreAST.CoreStepperHelpers.CoreLookup(findBindingForString)

type Binding = (Var, Expr Var)



spec :: Spec
spec = before bindingFinder $ do
    describe "demo tests" $ do
        it "can load example core expressions" $ \bindingFinder -> do
            putStr (showOutputable (bindingFinder "a"))
            --isTypeInformation TypeS `shouldBe` True

       -- it "defaults to false" $ do
       --     isTypeInformation 3 `shouldBe` False
        
    --describe "simplifyLiteral" $ do
        --it "can simplify float literals" $ do
            --  let expected = LitFloatS 3.5
            --let input = LitFloat 3.5
            -- simplifyLiteral input `shouldBe` expected
        --it "can simplify char literals" $ do
            -- let expected = LitCharS 'c'
            -- let input = LitFloat 3.5
            -- simplifyLiteral input `shouldBe` expected

                