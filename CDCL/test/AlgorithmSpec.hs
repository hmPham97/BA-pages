module AlgorithmSpec where
import           CDCL.Algorithm (cdcl, interpret, searchTuple)
import           CDCL.Types (Activity (..), BoolVal (..), CDCLResult (..),
                     InterpretResult (..), Level (..), Reason (..), Tuple,
                     Variable (..), transformClauseList)
import           Control.Exception (evaluate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
    describe "testing" $ do
        it "interpret should return 1" $
            interpret (transformClauseList[[1,2,-3],[-2],[4,5]])
            [((Variable 1,BFalse), Decision),((Variable 2,BFalse), Reason [Variable (-2)]),
            ((Variable 3,BFalse), Reason [Variable 1, Variable 2, Variable (-3)] ),
            ((Variable 4,BFalse), Decision),((Variable 5,BTrue), Reason [Variable 4, Variable 5])]
            `shouldBe` OK
        it "interpret should return -1" $
            interpret (transformClauseList[[1],[-1]]) [((Variable (-1),BNothing), Reason [Variable (-1)])] `shouldBe` UNRESOLVED
        it "interpret should return 0" $
            interpret (transformClauseList [[1,2,3,4],[-2,-3],[-4,-1]])
            [((Variable 1,BFalse), Reason [Variable (-4), Variable (-1)]), ((Variable 2,BTrue), Decision),
            ((Variable 3,BTrue), Reason [Variable (-2), Variable (-3)]),
            ((Variable 4,BTrue), Decision)] `shouldBe` NOK [Variable (-2), Variable (-3)]
        it "searchTuple should return 1" $
            searchTuple (Variable 3) [((Variable 1,BFalse), Decision),((Variable 2,BTrue), Decision),((Variable 3,BTrue), Decision),((Variable 4,BTrue), Decision)] `shouldBe` BTrue
        it "searchTuple should return 0" $
            searchTuple (Variable 1) [((Variable 1,BFalse), Decision),((Variable 2,BTrue), Decision),((Variable 3,BTrue), Decision),((Variable 4,BTrue), Decision)] `shouldBe` BFalse
        it "cdcl should return SAT [(2,1)] (fromList [(Level 1, [(Variable 1,0), (Variable 2,1)])]" $
            cdcl [[1,2]] `shouldBe` SAT [(Variable 1,BFalse),(Variable 2,BTrue)] (Map.fromList [(Level 1, [((Variable 1,BFalse), Decision),((Variable 2,BTrue), Reason [Variable 1, Variable 2])])])
        it "cdcl should return SAT [(2,0),(3,1),(4,0)] (fromList [(Level 1,[(Variable 1,0),(Variable 2,1)])])" $
            cdcl [[1,2,3,4],[-2],[2,3],[-4,-3]] `shouldBe` SAT [(Variable 2,BFalse),(Variable 3,BTrue),(Variable 4,BFalse)]
            (Map.fromList [(Level 0, [((Variable 2,BFalse), Reason [Variable (-2)]),((Variable 3,BTrue),Reason [Variable 2,Variable 3]), ((Variable 4,BFalse), Reason [Variable (-4), Variable (-3)])])])
        it "cdcl should return UNSAT" $ do
            cdcl [[1],[-1]] `shouldBe` UNSAT
