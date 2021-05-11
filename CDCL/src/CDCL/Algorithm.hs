---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Algorithm
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :   Apache-2.0
-- Maintainer  :
-- Stability   :
-- Portability :
-- = Description
-- General algorithms for DPLL and CDCL Algorithm.
-- Includes interpreting clauses, clauseLists,
-- tuple values and calculating ClauseList.
---------------------------------------------------------------------


module CDCL.Algorithm (cdcl, searchTuple, interpret) where

import           CDCL.Decisionalalgorithm (getHighestActivity,
                     getShortestClauseViaActivity, initialActivity,
                     setVariableViaActivity, updateActivity)

import           CDCL.Types (Activity (..), ActivityMap, BoolVal (..),
                     CDCLResult (..), Clause, ClauseList, InterpretResult (..),
                     Level (..), MappedTupleList, TriTuple, Tuple,
                     TupleClauseList, Variable (..), getEmptyClause, getNOK,
                     getVariableValue, increaseLvl, negateVariableValue,
                     transformClauseList)
import qualified CDCL.Types as TypeC

import           CDCL.Unitpropagation (unitPropagation, unitResolution,
                     unitSubsumption)

import           CDCL.MapLogic (pushToMappedTupleList)

import           CDCL.Conflict (analyzeConflict)

import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe

-- | This function will start the CDCL Function.
--   To call this function do for example:
--   cdcl [[1,2,3],[2,5]]
--   cdcl [[1,2,3,4], [2,4], [4,5],[3,6,7],[3,9,1],[3,8,10]]
--   The function will return the result of the cdcl' function.
cdcl :: [[Integer]] -> CDCLResult
cdcl clist = cdcl' aMap (Level 0) [] Map.empty transformedList transformedList
    where transformedList = transformClauseList clist
          aMap = initialActivity transformedList Map.empty

-- | Implementation not done
--   Function recursively calls itself until either following result happens
--   interpreted = 1 -> SAT TupleClauseList
--   interpreted = 0 and lvl = 0 -> UNSAT
cdcl' :: ActivityMap -> Level -> TupleClauseList -> MappedTupleList -> ClauseList -> ClauseList -> CDCLResult
cdcl' aMap (Level lvl)  tlist mappedTL clistOG clist
    | getNOK interpreted =
        let empty = getEmptyClause interpreted in
            let analyzelv = analyzeConflict (Level lvl) empty mappedTL clistOG in
                if fst analyzelv == Level (-1) then UNSAT
                else error "not implemented"--cdcl' aMap (Level (lvl -1)) tlist mappedTL clistOG (snd analyzelv)
            --cdcl' analyzelv clistOG clist tlist
    | interpreted == OK = SAT (map fst tupleRes) updatedMap
    | Level lvl > Level 10 = error "stop"
    | otherwise = cdcl' aMap newLvl list updateMapViaDecision clistOG (calculateClauseList (getFirstElem res) list)
    where res = unitPropagation clist tlist (Level lvl) mappedTL
          tupleRes = getSecondElem res
          updatedMap = getThirdElem res
          interpreted = interpret clistOG tupleRes
          newLvl = increaseLvl (Level lvl)
          highestActivity = getHighestActivity (getFirstElem res) aMap [(Variable 0, Activity 0)]
          shortestCl = getShortestClauseViaActivity (getFirstElem res) [] highestActivity
          firstShortestCl = head shortestCl
          assuredShortestClause = fst firstShortestCl
          firstHighestActivityInClause = getHighestActivity [firstShortestCl] aMap [(Variable 0, Activity 0)]
          decided = setVariableViaActivity assuredShortestClause (head firstHighestActivityInClause) -- Need change here? it takes first highest found VariableActivity in the clause.
          updateMapViaDecision = uncurry (pushToMappedTupleList updatedMap newLvl) decided
          list = nub (tlist ++ [decided] ++ tupleRes)

    -- if interpret clistOG (snd res) == 0 then -- checkEmptyClause needs to be changed. eventuell einfach interpret auf 0 checken?
    --     let empty = clist in -- function which calculates empty clause
    --         let analyzelv = lvl in -- function to analyze conflict
    --             -- backtrack-algorithm. will caculate new tlist or mapped tupleList
    --             cdcl' analyzelv clistOG clist tlist -- replace tlist with the backtrack algorithm
    -- else if interpret clistOG (snd res) == 1 then 1 else
    --     let newLvl = lvl + 1 in
    --         cdcl' newLvl clistOG (calculateClauseList (fst res) decided) decided
    --         where decided = [(2,1)] -- decided needs to become a proper function

-- | calculates the clauselist which will be given to unitpropagation.
--   returns when everything of tupelClauselist was calculated
calculateClauseList :: ClauseList -> TupleClauseList -> ClauseList
calculateClauseList cl tlist@(xs : ys)
    | null ys = reso
    | otherwise = calculateClauseList reso ys
    where sub = unitSubsumption cl xs
          reso = unitResolution sub xs

    --if checkEmptyClause clist then 1 else

-- | Bsp: [[2,1,3],[-1]] [(1,0),(3,0),(2,0)] -> 0. CONFLICT
--   Bsp: [[2,1,3]][(1,0),(2,0)] -> -1. Etwas wurde noch nicht belegt o. etwas wurde nicht positiv.
interpret :: ClauseList -> TupleClauseList -> InterpretResult
interpret t@(formel : xs) interpretation -- = do
    | null interpretation || interpreted == UNRESOLVED = UNRESOLVED
    | getNOK interpreted = NOK (snd formel)
    | not (null xs) = interpret xs interpretation
        --if value == UNRESOLVED then value else if value /= OK then NOK formel else interpret xs interpretation
    | otherwise = interpreted --interpret' (snd formel) interpretation False
    where interpreted = interpret' (snd formel) interpretation False

-- | Returns 1, 0 and -1
--   Interprets a single clause of a formula
--   Empty Clause if whole clause interprets to 0. if -1 appears it means the clause isnt finished from interpreting
interpret' :: Clause -> TupleClauseList -> Bool -> InterpretResult
interpret' (formel : xs) interpretation boolValue-- = do
    | tupelValue == BNothing   && null xs = UNRESOLVED
    | tupelValue == BNothing  && not (null xs) = interpret' xs interpretation True
    | (formelValue >= 0 && tupelValue == BTrue) || (formelValue < 0 && tupelValue == BFalse) = OK
    | ((formelValue >= 0 && tupelValue == BFalse) || (formelValue < 0 && tupelValue == BTrue))  && null xs && not boolValue= NOK  (formel :  xs)
    | boolValue && null xs = UNRESOLVED
    | otherwise = interpret' xs interpretation boolValue
        where formelValue = getVariableValue formel
              varValue = if formelValue < 0 then negateVariableValue formel else formel
              tupelValue = searchTuple varValue interpretation


-- | Get the set value from the tupelClauselist.
searchTuple :: Variable -> TupleClauseList -> BoolVal
searchTuple xval (xs : ys)
    | fst tuple == xval = snd tuple
    | not (null ys) = searchTuple xval ys
    | otherwise = BNothing
    where tuple = fst xs

searchTuple _ _ = BNothing

-- | returns the clauseList from unitPropagation
getFirstElem :: TriTuple -> ClauseList
getFirstElem (x, _, _) = x

-- | returns the TupleClauseList from unitPropagation
getSecondElem ::  TriTuple -> TupleClauseList
getSecondElem (_, x, _) = x

-- | returns the MappedTupleList from unitPropagation
getThirdElem :: TriTuple -> MappedTupleList
getThirdElem (_, _, x) = x
