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
        -- | Formula resolved, with TupelList to show how it was solved
        SAT TupelList MappedTupleList
    |
        -- | Formula not resolved
        UNSAT
    deriving(Eq, Ord)

instance Show CDCLResult where
    show (SAT tl mtl) = "SAT " ++ show tl ++ "\n\n" ++ show mtl ++ "\n"
    show UNSAT = "UNSAT"

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

-- | Variable defined as Integer
newtype Variable = Variable Integer
    deriving (Show, Eq, Ord)

-- | Level is associated with the decision level.
--   Defined as an Integer
newtype Level = Level Integer
    deriving (Show, Eq, Ord)

-- | Activity defined as Integer
newtype Activity = Activity Integer
    deriving (Show, Eq, Ord)

-- | Clause defined as a List of Variables
type Clause = [Variable]

-- | Tupel of 2 Clauses
--   First clause in tuple is reduced via Unitresolution
--   Second clause is the clause in its original form
type ReducedClauseAndOGClause = (Clause, Clause)

-- | ClauseList defined as a List of ReducedClauseAndOGClause
type ClauseList = [ReducedClauseAndOGClause]

-- | Tupel is defined as a Tupel of (Variables, Integer).
--   Integers in this case are only 0 or 1 valuewise.
type Tupel = (Variable, BoolVal)

-- | List containing Tupels.
type TupelList = [Tupel]

-- | A tuple of Tupel and Reason
type TupelClause = (Tupel, Reason)

-- | TupelList is a list of Tupels
type TupelClauseList = [TupelClause]

-- | Defined as Map.Map Integer TupelList
type MappedTupleList = Map.Map Level TupelClauseList

-- | Shows how often a variable is found in the formulas
-- | Defined as Map.Map Variable Activity
type ActivityMap = Map.Map Variable Activity

-- | Is a single Tupel containing the variable and activty
--   Defined as (Variable, Activty)
type VariableActivity = (Variable, Activity)

-- | Defined by using three Types.
--   These are ClauseList, TupelList and MappedTupleList
type TriTuple = (ClauseList , TupelClauseList, MappedTupleList)

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
getVariableValue (Variable x) = x

-- | Multiply the given Integervalue with -1
negateVariableValue :: Variable -> Variable
negateVariableValue (Variable x) = Variable (-x)

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
transformClauseList :: [[Integer]] -> ClauseList -> ClauseList
transformClauseList (xs : ys) cList
    | null ys = cList ++ [transformClause xs []]
    | otherwise = transformClauseList ys [transformClause xs []] ++ cList

-- | Transforms a list of Integers into a ReducedClauseAndOGClause
transformClause :: [Integer] -> Clause -> ReducedClauseAndOGClause
transformClause (xs : ys) varList
    | null ys = (varList ++ [Variable xs], varList ++ [Variable xs])
    | otherwise = transformClause ys (varList ++ [Variable xs])

-- | Checks if Interpretresult contains NOK.
--   Return true if it does, else false
getNOK :: InterpretResult -> Bool
getNOK (NOK _) = True
getNOK _ = False

-- | Returns the Clause which caused the NOK in InterpretResult
getEmptyClause :: InterpretResult -> Clause
getEmptyClause (NOK x) = x
