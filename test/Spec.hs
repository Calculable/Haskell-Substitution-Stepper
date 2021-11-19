import Test.Hspec
import Test.Hspec.QuickCheck
import GHC.Types.Literal

import SimplifiedCoreAST.SimplifiedCoreASTConverter
import SimplifiedCoreAST.SimplifiedCoreAST

main :: IO ()
main = hspec $ do
    describe "SimplifiedCoreASTConverter" $ do
        describe "isTypeInformation" $ do
            it "can identifie type" $ do
                isTypeInformation (TypeS) `shouldBe` True
            
            it "defaults to false" $ do
                isTypeInformation 3 `shouldBe` False