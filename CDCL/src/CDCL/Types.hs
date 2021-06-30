{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Types
-- Description :   Contains type declaration for CDCL Package
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :   Apache-2.0
-- Maintainer  :
-- Stability   :
-- Portability :
--
---------------------------------------------------------------------
module CDCL.Types where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Datatype for CDCL
data CDCLResult
    =
        -- | Formula resolved, with TupleList to show how it was solved
        SAT TupleList
    |
        SAT_WITH_STATS TupleList MappedTupleList Integer [Clause]
    |
        -- | Formula not resolved
        UNSAT
    |
        UNSAT_WITH_STATS [Clause] [Clause]
    deriving(Eq, Ord)

instance Show CDCLResult where
    show (SAT tl) = "Result:\nSAT " ++ show tl
    show (SAT_WITH_STATS tl mtl int1 learned) = "Result:\nSAT " ++ show tl ++ "\n\nStatistics:" ++ "\n\nDecisions:\n"
     ++ show mtl ++ "\n\nAmount of learned Clauses: " ++ show int1 ++ "\nLearned Clauses: " ++ show learned
    show UNSAT = "UNSAT"
    show (UNSAT_WITH_STATS cl conf) = "UNSAT. Learned Clauses: \n" ++ show cl  ++ "\nClauses which caused conflict:\n" ++ show conf

-- | Datatype for Reason
--   Shows if it was a decision or if the set Variable has a clause as Reason
--   for the set BoolVal
data Reason =
        -- | The algorithm decided that the variable will have the value it has
        Decision
    |
        -- | The algorithm calculated the BoolVal based on the other set BoolVal
        Reason Clause
    deriving (Show, Eq, Ord)

-- | Datatyp for BoolVal
--   Is used to show which value the set Variable has
data BoolVal =
        -- | Valuewise 0
        BFalse
    |
        -- | Valuewise 1
        BTrue
    |
        -- | Valuewise -1
        BNothing
    deriving (Show, Eq, Ord)

-- | Datatype for InterpResult
data InterpretResult =
        -- | Valuewise 1. Clause is solved
        OK
    |
        -- | Valuewise 0. The conflict clause will be shown in Clause
        NOK Clause
    |
        -- | Valuewise -1. A variable isn't set, which can solve the clause.
        UNRESOLVED
    deriving (Show, Eq, Ord)

data Origin =

        ORIGINAL
    |
        LEARNED
    |
        ERR
    deriving (Show, Eq, Ord)

-- | Variable defined as Integer
newtype Variable = Var Integer
    deriving (Show, Eq, Ord)

-- | Level is associated with the decision level.
--   Defined as an Integer
newtype Level = Level Integer
    deriving (Show, Eq, Ord)

-- | Activity defined as Integer
newtype Activity = Activity Integer
    deriving (Show, Eq, Ord)

-- | Period. If Period is 0 half the current activitymap
newtype Period = Period Integer
    deriving (Eq)

-- | Clause defined as a List of Variables
type Clause = [Variable]

-- | Tuple of 2 Clauses
--   First clause in tuple is reduced via Unitresolution
--   Second clause is the clause in its original form
type ReducedClauseAndOGClause = (Clause, Clause, Origin)

type LearnedClauseList = [(Clause, Origin)]

-- | ClauseList defined as a List of ReducedClauseAndOGClause
type ClauseList = [ReducedClauseAndOGClause]

-- | Tuple is defined as a Tuple of (Variables, Integer).
--   Integers in this case are only 0 or 1 valuewise.
type Tuple = (Variable, BoolVal)

-- | List containing Tupels.
type TupleList = [Tuple]

-- | A tuple of Tuple and Reason
type TupleClause = (Tuple, Reason)

-- | TupleClauseList is a list of TupleClause
type TupleClauseList = [TupleClause]

-- | Defined as Map.Map Integer TupleList
type MappedTupleList = Map.Map Level TupleClauseList

-- | Shows how often a variable is found in the formulas
-- | Defined as Map.Map Variable Activity
type ActivityMap = Map.Map Variable Activity

-- | Is a single Tuple containing the variable and activty
--   Defined as (Variable, Activty)
type VariableActivity = (Variable, Activity)

-- | Defined by using three Types.
--   These are ClauseList, TupelClauseList and MappedTupleList
type TriTuple = (ClauseList , TupleClauseList, MappedTupleList)

-- | Increase the given level by one
increaseLvl :: Level -> Level
increaseLvl (Level i) = Level (i + 1)

-- | Decrease the given level by one
decreaseLvl :: Level -> Level
decreaseLvl (Level i) = Level (i - 1)

-- | Get the current level
getLevel :: Level -> Integer
getLevel (Level i) = i

-- | Get the Integervalue of the given Variable
getVariableValue :: Variable -> Integer
getVariableValue (Var x) = x

-- | Multiply the given Integervalue with -1
negateVariableValue :: Variable -> Variable
negateVariableValue (Var x) = Var (-x)

-- | Get the Integervalue of the given Activity
getActivityValue :: Activity -> Integer
getActivityValue (Activity i) = i

-- | Increase the Activity by one
increaseActivity :: Activity -> Activity
increaseActivity (Activity i) = Activity (i + 1)

-- | Divide the activity by 2. Round the Value down
divideActivity :: Activity -> Activity
divideActivity (Activity i) = Activity (i `div` 2)

-- | Transforms a given List of Integerlists into a ClauseList.
transformClauseList :: [[Integer]] -> ClauseList
transformClauseList (xs : ys)
    | null ys = [transformClause xs []]
    | otherwise = transformClause xs [] : transformClauseList ys
-- | Transforms a list of Integers into a ReducedClauseAndOGClause
transformClause :: [Integer] -> Clause -> ReducedClauseAndOGClause
transformClause (xs : ys) varList
    | null ys = (Var xs : varList, Var xs : varList, ORIGINAL)
    | otherwise = transformClause ys (Var xs : varList)
-- | Checks if Interpretresult contains NOK.
--   Return true if it does, else false
getNOK :: InterpretResult -> Bool
getNOK (NOK _) = True
getNOK _ = False

-- | Returns the Clause which caused the NOK in InterpretResult
getEmptyClause :: InterpretResult -> Clause
getEmptyClause (NOK x) = x

-- | Returns the Clause in Reason
getReason :: Reason -> Clause
getReason (Reason r) = r

-- | Decrease a given Period by 1
decreasePeriod :: Period -> Period
decreasePeriod (Period r) = Period (r - 1)

getClauseFromReducedClauseAndOGClause :: ReducedClauseAndOGClause -> Clause
getClauseFromReducedClauseAndOGClause (x, _, _) = x

getOGFromReducedClauseAndOGClause :: ReducedClauseAndOGClause -> Clause
getOGFromReducedClauseAndOGClause (_, x, _) = x

getOriginFromReducedClauseAndOGClause :: ReducedClauseAndOGClause -> Origin
getOriginFromReducedClauseAndOGClause (_, _, x) = x

transformToLearnedClauses :: ClauseList -> [[Integer]] -> [[Integer]]
-- transformToLearnedClauses ys learned
--   = foldl
--       (\ learned xs
--          -> getOGFromReducedClauseAndOGClause xs
--               : learned)
--       learned ys
transformToLearnedClauses (xs : ys) learnedList = transformToLearnedClauses ys (map getVariableValue (getOGFromReducedClauseAndOGClause xs) : learnedList)
transformToLearnedClauses [] learned = learned