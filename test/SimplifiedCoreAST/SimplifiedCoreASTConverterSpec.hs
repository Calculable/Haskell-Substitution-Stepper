module SimplifiedCoreAST.SimplifiedCoreASTConverterSpec where
    
import Test.Hspec
import Test.Hspec.QuickCheck

import SimplifiedCoreAST.SimplifiedCoreASTConverter
import SimplifiedCoreAST.SimplifiedCoreAST




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

                