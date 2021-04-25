---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Types
-- Description :   Contains type declaration for CDCL Package
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :
-- Maintainer  :
-- Stability   :
-- Portability :
--
---------------------------------------------------------------------
module CDCL.Types where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Datatyp for DPLL.
data DPLLResult
    =
        -- | DPLL is resolved
        DResolved
    |
        -- | DPLL is not resolved
        DNotResolved
    deriving(Eq, Ord, Show)

-- | Datatyp for CDCL
data CDCLResult
    =
        -- | Formula resolved, with TupelList to show how it was solved
        SAT TupelList MappedTupleList
    |
        -- | Formula not resolved
        UNSAT
    deriving(Eq, Ord, Show)

-- | Variable defined as Integer
type Variable = Integer

-- | Clause defined as a List of Variables
type Clause = [Variable]

-- | ClauseList defined as a List of Clauses
type ClauseList = [Clause]

-- | Tupel is defined as a Tupel of (Variables, Integer).
--   Integers in this case are only 0 or 1 valuewise.
type Tupel = (Variable, Integer)

-- | TupelList is a list of Tupels
type TupelList = [Tupel]

-- | Level is associated with the decision level.
--   Defined as an Integer
type Level = Integer

-- | Defined as Map.Map Integer TupelList
type MappedTupleList = Map.Map Integer TupelList

-- | Activity defined as Integer
type Activity = Integer

-- | Shows how often a variable is found in the formulas
-- | Defined as Map.Map Variable Activity
type ActivityMap = Map.Map Variable Activity

-- | Is a single Tupel containing the variable and actiivty
--   Defined as (Variable, Actiivty)
type VariableActivity = (Variable, Activity)

-- | Defined by using three Types.
--   These are ClauseList, TupelList and MappedTupleList
type TriTuple = (ClauseList , TupelList, MappedTupleList)