---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Unitpropagation
-- Description :   Contains necessary logic for unitpropagation
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :   Apache-2.0
-- Maintainer  :
-- Stability   :
-- Portability :
--
---------------------------------------------------------------------
module CDCL.Unitpropagation (getUnitClause, unitSubsumption,
    unitResolution, unitPropagation) where

import           CDCL.Types (BoolVal (..), Clause, ClauseList, Level,
                     MappedTupleList, Reason (..), ReducedClauseAndOGClause,
                     TriTuple, Tuple, TupleClauseList, getVariableValue, 
                     negateVariableValue, getOGFromReducedClauseAndOGClause,
                     getClauseFromReducedClauseAndOGClause)
import qualified CDCL.Types as TypesC

import           CDCL.MapLogic (pushToMappedTupleList)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | The function is the base for the unitpropagation procedure. It checks first if an
--   unitclause exists. If it does, it will set the variable so that the unitclause is solved.
unitPropagation :: ClauseList -> TupleClauseList -> Level -> MappedTupleList -> TriTuple
unitPropagation clist tlist lvl mapped

    -- Case: no UnitClause found or no more clauses in ClauseList
    | null clist || null fstElem = (clist, tlist, mapped)
    | otherwise = unitPropagation resolutionC (tlist ++ [(calcTuple, ogClause)]) lvl updatedMap
    where 
          unitClause = getUnitClause clist
          fstElem = getClauseFromReducedClauseAndOGClause unitClause
          calcTuple = setVariable fstElem
          ogClause = Reason (getOGFromReducedClauseAndOGClause unitClause)
          updatedMap = pushToMappedTupleList mapped lvl calcTuple ogClause
          subsumptionC = unitSubsumption clist calcTuple
          resolutionC = unitResolution subsumptionC calcTuple

-- | checks if an unit clause exists in the given list of lists. if one exists return the list.
getUnitClause :: ClauseList  -> ReducedClauseAndOGClause
getUnitClause (clause : xs) = let listLength = length (getClauseFromReducedClauseAndOGClause clause) in
    if listLength == 1 then clause else getUnitClause xs

getUnitClause _ = ([],[])

-- | call this method on unit clauses only. If the value is less then 0 set a 0 in the tuple, else set 1
setVariable :: Clause  -> Tuple
setVariable clause = if getVariableValue (head clause) < 0
    then (negateVariableValue (head clause), BFalse) else (head clause, BTrue) -- Need change here

-- | Remove clauses which have removableVar as variable.
unitSubsumption :: ClauseList  -> Tuple -> ClauseList
unitSubsumption (firstList : xs) tuple

    -- Case: Variable is not found in current clause. Readd it to the ClauseList
    | not checked = firstList : unitSubsumption xs tuple

    -- Case: Variable was found. Remove the clause.
    | otherwise = unitSubsumption xs tuple
    where val = if snd tuple == BTrue then fst tuple else negateVariableValue (fst tuple)
          checked = val `elem` getClauseFromReducedClauseAndOGClause firstList -- checks if val is inside list

unitSubsumption _ _ = []

-- | remove negated variable of the variable which was set
--   For example a negated variable was resolved, which would remove
--   the positive ones.
unitResolution :: ClauseList -> Tuple -> ClauseList
unitResolution (firstList : xs) tuple

    -- Case: Variable is not found in the current Clause. Dont adjust it.
    | not checked = firstList : unitResolution xs tuple

    -- Case: Variable was found in current clause. Adjust the clause and readd it.
    | otherwise = let list = filter (/= val) (getClauseFromReducedClauseAndOGClause firstList) in
        (list, ogClause) : unitResolution xs tuple
    where val = if snd tuple == BFalse then fst tuple else negateVariableValue (fst tuple)
          checked = val `elem` getClauseFromReducedClauseAndOGClause firstList -- checks if val is inside list
          ogClause = getOGFromReducedClauseAndOGClause firstList

unitResolution _ _ = []
