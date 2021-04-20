module AlgorithmSpec where
import           CDCL.Algorithm (cdcl, dpll, interpret, searchTupel)
import           CDCL.Types (CDCLResult (..), DPLLResult (..))
import           Control.Exception (evaluate)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
    describe "testing" $ do
        it "dpll test should return DResolved" $
            dpll [[1]] [] `shouldBe` DResolved
        it "dpll test should return DNotResolved" $
            dpll [[1],[-1]] [] `shouldBe` DNotResolved
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
        it "cdcl should return SAT [(2,1)]" $
            cdcl [[1,2]] [] `shouldBe` SAT [(1,0),(2,1)]
        it "cdcl should return SAT [(2,0),(3,1),(4,0)]" $
            cdcl [[1,2,3,4],[-2],[2,3],[-4,-3]] [] `shouldBe` SAT [(2,0),(3,1),(4,0)]
        it "cdcl should throw error not implemented" $ do
            evaluate(cdcl [[1],[-1]] []) `shouldThrow` anyException
