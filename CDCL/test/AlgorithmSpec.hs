module AlgorithmSpec where
import Algorithm (dpll, interpret, searchTupel, cdcl)
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

spec :: Spec 
spec =
    describe "testing" $ do
        it "dpll test should return 1" $
            dpll [[1]] [] `shouldBe` 1
        it "dpll test should return -1" $
            dpll [[1],[-1]] [] `shouldBe` (-1)
        it "interpret should return 1" $
            interpret [[1,2,-3],[-2],[4,5]] [(1,0),(2,0),(3,0),(4,0),(5,1)] `shouldBe` 1
        it "interpret should return -1" $
            interpret [[1],[-1]] [(-1,-1)] `shouldBe` (-1)
        it "interpret should return 0" $
            interpret [[1,2,3,4],[-2,-3],[-4,-1]] [(1,0),(2,1),(3,1),(4,1)] `shouldBe` 0
        it "searchTupel should return 1" $
            searchTupel 3 [(1,0),(2,1),(3,1),(4,1)] `shouldBe` 1
        it "searchTupel should return 0" $
            searchTupel 1 [(1,0),(2,1),(3,1),(4,1)] `shouldBe` 0
        it "cdcl should return 1" $
            cdcl [[1,2]] [] `shouldBe` 1
        it "cdcl should return 1" $
            cdcl [[1,2,3,4],[-2],[2,3],[-4,-3]] [] `shouldBe` 1
        it "cdcl should throw error not implemented" $ do
            evaluate(cdcl [[1],[-1]] []) `shouldThrow` anyException
        it "cdcl should throw error stop" $ do
            evaluate(cdcl [[1,23]] []) `shouldThrow` anyException       