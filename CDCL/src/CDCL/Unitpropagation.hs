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
                     TriTuple, Tuple, TupleClause, TupleClauseList,
                     getVariableValue, negateVariableValue)
import qualified CDCL.Types as TypesC

import           CDCL.MapLogic (pushToMappedTupleList)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | The function is the base for the unitpropagation procedure. It checks first if an 
--   unitclause exists. If it does, it will set the variable so that the unitclause is solved.
-- 
unitPropagation :: ClauseList -> TupleClauseList -> Level -> MappedTupleList -> TriTuple
unitPropagation clist tlist lvl mapped
    | null clist || null (fst unitClause) = (clist, tlist, mapped)
    | otherwise = unitPropagation resolutionC (tlist ++ [(calcTuple, ogClause)]) lvl updatedMap
    where unitClause = getUnitClause clist
          calcTuple = setVariable (fst unitClause)
          fstTuple = fst calcTuple
          ogClause = Reason (snd unitClause)
          updatedMap = pushToMappedTupleList mapped lvl calcTuple ogClause
          subsumptionC = unitSubsumption clist (calcTuple, ogClause)
          resolutionC = unitResolution subsumptionC (calcTuple, ogClause)

-- | checks if an unit clause exists in the given list of lists. if one exists return the list.
getUnitClause :: ClauseList  -> ReducedClauseAndOGClause
getUnitClause (clause : xs) = let listLength = length (fst clause) in
    if listLength == 1 then clause else getUnitClause xs

getUnitClause _ = ([],[])

-- | call this method on unit clauses only. If the value is less then 0 set a 0 in the tuple, else set 1
setVariable :: Clause  -> Tuple
setVariable clause = if getVariableValue (head clause) < 0
    then (negateVariableValue (head clause), BFalse) else (head clause, BTrue) -- Need change here

-- | Remove clauses which have removableVar as variable.
unitSubsumption :: ClauseList  -> TupleClause -> ClauseList
unitSubsumption (firstList : xs) tuple
    | not checked = filter (not . null) (firstList : unitSubsumption xs tuple)
    | otherwise = filter (not . null) (unitSubsumption xs tuple)
    where val = if snd (fst tuple) == BTrue then fst (fst tuple) else negateVariableValue (fst (fst tuple))
          checked = val `elem` fst firstList -- checks if val is inside list

unitSubsumption _ _ = []

-- | remove negated variable of the variable which was set
--   For example a negated variable was resolved, which would remove
--   the positive ones.
unitResolution :: ClauseList -> TupleClause -> ClauseList
unitResolution (firstList : xs) tuple
    | not checked = filter (not . null) (firstList : unitResolution xs tuple)
    | otherwise = let list = filter (/= val) (fst firstList) in
        filter (not . null) ((list,snd firstList) : unitResolution xs tuple)
    where val = if snd (fst tuple) == BFalse then fst (fst tuple) else negateVariableValue (fst (fst tuple))
          checked = val `elem` fst firstList -- checks if val is inside list

unitResolution _ _ = []
