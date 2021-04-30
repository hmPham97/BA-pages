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

-- | Datatyp for CDCL
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

data Reason = 
        Decision
    |
        Reason Clause
    deriving (Show, Eq, Ord)

-- | Found as second value in Tupel
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

data InterpretResult =
        OK
    |
        NOK Clause
    |
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

type TupelList = [Tupel]

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

increaseLvl :: Level -> Level
increaseLvl (Level i) = Level (i + 1)

getLevel :: Level -> Integer
getLevel (Level i) = i

getVariableValue :: Variable -> Integer
getVariableValue (Variable x) = x

negateVariableValue :: Variable -> Variable
negateVariableValue (Variable x) = Variable (-x)

getActivityValue :: Activity -> Integer
getActivityValue (Activity i) = i

increaseActivity :: Activity -> Activity
increaseActivity (Activity i) = Activity (i + 1)

transformClauseList :: [[Integer]] -> ClauseList -> ClauseList
transformClauseList (xs : ys) cList
    | null ys = cList ++ [transformClause xs []]
    | otherwise = transformClauseList ys [transformClause xs []] ++ cList

transformClause :: [Integer] -> Clause -> ReducedClauseAndOGClause
transformClause (xs : ys) varList
    | null ys = (varList ++ [Variable xs], varList ++ [Variable xs])
    | otherwise = transformClause ys (varList ++ [Variable xs])

getNOK :: InterpretResult -> Bool 
getNOK (NOK _) = True 
getNOK _ = False 

getEmptyClause :: InterpretResult -> Clause
getEmptyClause (NOK x) = x