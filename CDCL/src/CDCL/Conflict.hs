module CDCL.Conflict (analyzeConflict) where

import CDCL.Types (Level(..), Reason(..),Clause, ClauseList, Variable(..), 
                  MappedTupleList, getLevel, getReason, TupleClauseList, ActivityMap)

import CDCL.Decisionalalgorithm (updateActivity)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe

-- | Example:
--  f = analyzeConflict  (Level 1) [Variable (-2), Variable (-3)] (Map.fromList [(Level 1, [((Variable 2, BTrue), Decision), ((Variable 3, BTrue), Reason [Variable (-2), Variable 3])])]) 
--  [([Variable 1, Variable 2], [Variable 1, Variable 2]), ([Variable (-2), Variable 3], [Variable (-2), Variable 3]), ([Variable(-2), Variable (-3)], [Variable (-2), Variable (-3)])]
analyzeConflict :: Level -> Clause -> MappedTupleList -> ClauseList-> (Level, ClauseList)
analyzeConflict lvl emptyClause mtl cList 
    | getLevel lvl == 0 = (Level (-1), cList)
    | otherwise = (lvl, cList ++ [(reason, reason)])
    where reason = calcReason lvl emptyClause mtl

-- | Calculate the reason of conflict
--   E.G. 
--   calcReason (Level 1) [Variable 1] (Map.fromList [(Level 1, [((Variable 1, BFalse), Reason [(Variable 1)])])])
--   calcReason (Level 1) [Variable 1] (Map.fromList [(Level 1, [((Variable 1, BFalse), Decision)])])
--
--   calcReason (Level 1) [Variable 1, Variable 2] (Map.fromList [(Level 1, [((Variable 1, BFalse), Decision), 
---  ((Variable 2, BTrue), Reason [Variable 2])])])

-- calcReason (Level 1) [Variable (-2), Variable (-3)] (Map.fromList [(Level 1, [((Variable 2, BTrue), Decision), ((Variable 3, BTrue), Reason [Variable (-2), Variable 3])])])
calcReason :: Level -> Clause -> MappedTupleList -> Clause
calcReason lvl emptyClause mtl 
    | getLevel lvl == 0 = emptyClause
    | fst tpl `elem` emptyClause = if snd lastVal /= Decision then
        getReason (snd lastVal) else search x emptyClause
    | otherwise  = emptyClause
    where associated = Map.lookup lvl mtl
          x = fromMaybe [] associated
          lastVal = last x
          tpl = fst lastVal

-- | help function
--   searches for tuple which is in clause.
--   needs to be changed, so that it first searches backwards.
search :: TupleClauseList -> Clause -> Clause 
search (xs : ys) cl 
    | fst (fst xs) `elem` cl && snd xs /= Decision = getReason (snd xs)  
    | otherwise = search ys cl

-- | Add the newly calculated Clause to the ClauseList and 
--   update the ActivityMap
addClause :: Clause -> ClauseList -> ActivityMap  -> (ClauseList, ActivityMap)
addClause cl cList aMap 
    | null cl = (cList, aMap)
    | otherwise = (updated, updatedAMap)
    where updated = (cl, cl) : cList
          updatedAMap = updateActivity cl aMap

-- | Creates the new clause. Works by applying union to 
--   empty clause with the reason. Also calls function to remove
--   the variable which causes the conflict
unionClause :: Clause -> Clause -> Variable -> Clause
unionClause cl1 cl2 v = let list = cl1 `union` cl2 in
    removeVariable list v

-- | remove the variable which was the cause for the conflict from
--   the clause. 
removeVariable :: Clause -> Variable -> Clause 
removeVariable [] v = []
removeVariable clause@(xs : ys) v 
    | xs == v = ys
    | otherwise = xs : removeVariable ys v

-- | cl1 is the learned clause. cl2 is the clause which was used to union
--   with another clause. 1UIP is First Unique Implication Point
--   Returns true if the learned clause is shorter then cl2.
is1UIP :: Clause -> Clause -> Bool 
is1UIP cl1 cl2 
    | length cl1 >= length cl2 = False 
    | otherwise = True