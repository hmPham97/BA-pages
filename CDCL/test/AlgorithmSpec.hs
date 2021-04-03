module AlgorithmSpec where
import Algorithm (dpll, interpret, searchTupel)
import Test.Hspec
import Test.QuickCheck

spec :: Spec 
spec =
    describe "dpll" $ do
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
        