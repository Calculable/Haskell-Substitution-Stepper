module OriginalCoreAST.CoreInformationExtractorFunctionsSpec where
    
import Test.Hspec
import Test.Hspec.QuickCheck
import DataProvider.DataProvider(exampleExpression)

import OriginalCoreAST.CoreInformationExtractorFunctions()
import Utils (showOutputable)
import GHC.Plugins (liftIO )
spec :: Spec
spec = do
    describe "demo tests" $ do
        it "can load example core expressions" $ do
            unwrappExpression <- exampleExpression
            putStr (showOutputable unwrappExpression)
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

                