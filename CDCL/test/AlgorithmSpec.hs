module AlgorithmSpec where
import           CDCL.Algorithm (cdcl, interpret, searchTupel)
import           CDCL.Types (CDCLResult (..), Variable(..), 
                 Activity(..), Level(..), BoolVal(..), transformClauseList)
import           Control.Exception (evaluate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
    describe "testing" $ do
        it "interpret should return 1" $
            interpret (transformClauseList[[1,2,-3],[-2],[4,5]] []) [(Variable 1,BFalse),(Variable 2,BFalse),(Variable 3,BFalse),(Variable 4,BFalse),(Variable 5,BTrue)] `shouldBe` 1
        it "interpret should return -1" $
            interpret (transformClauseList[[1],[-1]] []) [(Variable (-1),BNothing)] `shouldBe` (-1)
        it "interpret should return 0" $
            interpret (transformClauseList [[1,2,3,4],[-2,-3],[-4,-1]] []) [(Variable 1,BFalse),(Variable 2,BTrue),(Variable 3,BTrue),(Variable 4,BTrue)] `shouldBe` 0
        it "searchTupel should return 1" $
            searchTupel 3 [(Variable 1,BFalse),(Variable 2,BTrue),(Variable 3,BTrue),(Variable 4,BTrue)] `shouldBe` BTrue
        it "searchTupel should return 0" $
            searchTupel 1 [(Variable 1,BFalse),(Variable 2,BTrue),(Variable 3,BTrue),(Variable 4,BTrue)] `shouldBe` BFalse
        it "cdcl should return SAT [(2,1)] (fromList [(Level 1, [(Variable 1,0), (Variable 2,1)])]" $
            cdcl [[1,2]] `shouldBe` SAT [(Variable 1,BFalse),(Variable 2,BTrue)] (Map.fromList [(Level 1, [(Variable 1,BFalse),(Variable 2,BTrue)])])
        it "cdcl should return SAT [(2,0),(3,1),(4,0)] (fromList [(Level 1,[(Variable 1,0),(Variable 2,1)])])" $
            cdcl [[1,2,3,4],[-2],[2,3],[-4,-3]] `shouldBe` SAT [(Variable 2,BFalse),(Variable 3,BTrue),(Variable 4,BFalse)] (Map.fromList [(Level 0, [(Variable 2,BFalse),(Variable 3,BTrue),(Variable 4,BFalse)])])
        it "cdcl should throw error not implemented" $ do
            evaluate(cdcl [[1],[-1]]) `shouldThrow` anyException
