module AlgorithmSpec where
import           CDCL.Algorithm (cdcl, interpret, searchTuple)
import           CDCL.Conflict (calcReason)
import           CDCL.Types (Activity (..), BoolVal (..), CDCLResult (..),
                     InterpretResult (..), Level (..), Reason (..), Tuple,
                     Variable (..), transformClauseList)
import           Control.Exception (evaluate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.Hspec

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Data.Coerce
import qualified Picosat as PicoSAT

spec :: Spec
spec = do
    -- describe "testing" $ do
    --     it "interpret should return OK" $
    --         interpret (transformClauseList[[1,2,-3],[-2],[4,5]])
    --         [((Var 1,BFalse), Decision),((Var 2,BFalse), Reason [Var (-2)]),
    --         ((Var 3,BFalse), Reason [Var 1, Var 2, Var (-3)] ),
    --         ((Var 4,BFalse), Decision),((Var 5,BTrue), Reason [Var 4, Var 5])]
    --         `shouldBe` OK
    --     it "interpret should return UNRESOLVED" $
    --         interpret (transformClauseList[[1],[-1]]) [((Var (-1),BNothing), Reason [Var (-1)])] `shouldBe` UNRESOLVED
    --     it "interpret should return 0´NOK" $
    --         interpret (transformClauseList [[1,2,3,4],[-2,-3],[-4,-1]])
    --         [((Var 1,BFalse), Reason [Var (-4), Var (-1)]), ((Var 2,BTrue), Decision),
    --         ((Var 3,BTrue), Reason [Var (-2), Var (-3)]),
    --         ((Var 4,BTrue), Decision)] `shouldBe` NOK [Var (-2), Var (-3)]
    --     it "searchTuple should return BTrue" $
    --         searchTuple (Var 3) [((Var 1,BFalse), Decision),((Var 2,BTrue), Decision),((Var 3,BTrue), Decision),((Var 4,BTrue), Decision)] `shouldBe` BTrue
    --     it "searchTuple should return BFalse" $
    --         searchTuple (Var 1) [((Var 1,BFalse), Decision),((Var 2,BTrue), Decision),((Var 3,BTrue), Decision),((Var 4,BTrue), Decision)] `shouldBe` BFalse
        -- it "cdcl should return SAT [(2,1)] (fromList [(Level 1, [(Var 1,0), (Var 2,1)])]" $
        --     cdcl [[1,2]] `shouldBe` SAT [(Var 2,BFalse),(Var 1,BTrue)] (Map.fromList [(Level 1,[((Var 2,BFalse),Decision),((Var 1,BTrue),Reason [Var 1,Var 2])])]) 0
        -- it "cdcl should return SAT [(2,0),(3,1),(4,0)] (fromList [(Level 1,[(Var 1,0),(Var 2,1)])])" $
        --     cdcl [[1,2,3,4],[-2],[2,3],[-4,-3]] `shouldBe` SAT [(Var 2,BFalse),(Var 3,BTrue),(Var 4,BFalse)]
        --     (Map.fromList [(Level 0, [((Var 2,BFalse), Reason [Var (-2)]),((Var 3,BTrue),Reason [Var 2,Var 3]), ((Var 4,BFalse), Reason [Var (-4), Var (-3)])])]) 0
        -- it "cdcl should return UNSAT" $
        --     cdcl [[1],[-1]] `shouldBe` UNSAT
        -- it "calcReason should return [Var 1]" $
        --     calcReason (Level 1) [Var 1, Var 2] (Map.fromList[(Level 1, [((Var 1, BFalse), Decision), ((Var 2, BFalse), Reason [Var 1, Var (-2)])])]) `shouldBe` [Var 1]
        -- it "cdcl should return SAT [Var 1]" $
        --     cdcl [[1,2],[1,-2]] `shouldBe` SAT [(Var 2, BTrue),(Var 1,BTrue)] (Map.fromList [(Level 1, [((Var 2, BTrue), Decision), ((Var 1, BTrue), Reason [Var 1, Var (-2)])])]) 0
        -- it "cdcl should return UNSAT" $
        --     cdcl [[1,2],[1,-2],[-1,2],[-1,-2]] `shouldBe` UNSAT

    describe "Compare with picoSAT solver" $ do
        it "compare SAT / UNSAT results" $ do
          property $ \clauses -> prop_picoSATcomparison clauses



prop_picoSATcomparison :: [[NonZero Int]] -> Property
prop_picoSATcomparison cl = withMaxSuccess 1000 $ monadicIO $ do
  let clauses = coerce cl
  picoSol <- run $ PicoSAT.solve clauses
  let cdclSol = cdcl (map (map fromIntegral) clauses) False
  assert $ case (picoSol, cdclSol) of
             (PicoSAT.Unsatisfiable, UNSAT) -> True
             (PicoSAT.Unknown, _)           -> False
             (PicoSAT.Solution _, SAT _)   -> True
             _                              -> False
