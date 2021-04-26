---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Algorithm
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :   Apache 2.0
-- Maintainer  :
-- Stability   :
-- Portability :
-- = Description
-- General algorithms for DPLL and CDCL Algorithm.
-- Includes interpreting clauses, clauseLists,
-- tuple values and calculating ClauseList.
---------------------------------------------------------------------


module CDCL.Algorithm (interpret, searchTupel, cdcl, cdcl', calculateClauseList) where

import           CDCL.Decisionalgorithm (getHighestActivity,
                     getHighestActivity', getShortestClause,
                     getShortestClauseViaActivity, initialActivity,
                     setVariableViaActivity, updateActivity)
import           CDCL.Types (ActivityMap, CDCLResult (..), Clause, ClauseList, Variable(..),
                     Level(..), MappedTupleList, Tupel, TupelList, TriTuple,
                     Activity(..), BoolVal(..), increaseLvl, getVariableValue, 
                     negateVariableValue, transformClauseList)
import           CDCL.Unitpropagation (checkSetVariable, pushToMappedTupleList,
                     unitResolution, unitSubsumption)
import           CDCL.UPcdcl (unitPropagation)
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe

-- | This function will start the CDCL Function.
--   To call this function do for example:
--   cdcl [[1,2,3],[2,5]]
--   The function will return the result of the cdcl' function.
cdcl :: [[Integer]] -> CDCLResult
cdcl clist = cdcl' aMap (Level 0) [] Map.empty transformedList transformedList
    where transformedList = transformClauseList clist []
          aMap = initialActivity transformedList Map.empty

-- | Implementation not done
--   Function recursively calls itself until either following result happens
--   interpreted = 1 -> SAT TupelList
--   interpreted = 0 and lvl = 0 -> UNSAT
cdcl' :: ActivityMap -> Level -> TupelList -> MappedTupleList -> ClauseList -> ClauseList -> CDCLResult
cdcl' aMap (Level lvl)  tlist mappedTL clistOG clist
    | interpreted == 0 = let empty = clist in
        let analyzelv = lvl in
            error "not implemented"
            --cdcl' analyzelv clistOG clist tlist
    | interpreted == 1 = SAT tupleRes updatedMap
    | Level lvl > Level 10 = error "stop"
    | otherwise = cdcl' aMap newLvl list updateMapViaDecision clistOG (calculateClauseList (getFirstElem res) list)
    where res = unitPropagation clist tlist (Level lvl) mappedTL
          tupleRes = getSecondElem res
          updatedMap = getThirdElem res
          interpreted = interpret clistOG tupleRes
          newLvl = increaseLvl (Level lvl)
          shortestClauses = getShortestClause (getFirstElem res) []
          highestActivity = getHighestActivity shortestClauses aMap (Variable 0, Activity 0)
          shortestCl = getShortestClauseViaActivity shortestClauses highestActivity
          decided = setVariableViaActivity (fromMaybe [] shortestCl) highestActivity -- Need change here
          updateMapViaDecision = pushToMappedTupleList mappedTL newLvl decided
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
--   returns when everything of tupellist was calculated
calculateClauseList :: ClauseList -> TupelList -> ClauseList
calculateClauseList cl tlist@(xs : ys)
    | null xs = cl
    | null ys = reso
    | otherwise = calculateClauseList reso ys
    where sub = unitSubsumption cl xs
          reso = unitResolution sub xs

    --if checkEmptyClause clist then 1 else

-- | Bsp: [[2,1,3],[-1]] [(1,0),(3,0),(2,0)] -> 0. CONFLICT
--   Bsp: [[2,1,3]][(1,0),(2,0)] -> -1. Etwas wurde noch nicht belegt o. etwas wurde nicht positiv.
interpret :: ClauseList -> TupelList -> Integer
interpret t@(formel : xs) interpretation -- = do
    | null interpretation = -1
    | not (null xs) = let value = interpret' formel interpretation False in
        if value /= 1 then value else interpret xs interpretation
    | otherwise = interpret' formel interpretation False

-- | Returns 1, 0 and -1
--   Interprets a single clause of a formula
--   Empty Clause if whole clause interprets to 0. if -1 appears it means the clause isnt finished from interpreting
interpret' :: Clause -> TupelList -> Bool -> Integer
interpret' (formel : xs) interpretation x-- = do
    | tupelValue == BNothing   && null xs = -1
    | tupelValue == BNothing  && not (null xs) = interpret' xs interpretation True
    | (formelValue >= 0 && tupelValue == BTrue) || (formelValue < 0 && tupelValue == BFalse) = 1
    | ((formelValue >= 0 && tupelValue == BFalse) || (formelValue < 0 && tupelValue == BTrue))  && null xs && not x= 0
    | x && null xs = -1
    | otherwise = interpret' xs interpretation x
        where formelValue = getVariableValue formel
              varValue = if formelValue < 0 then negateVariableValue formel else formel
              tupelValue = searchTupel (getVariableValue varValue) interpretation


-- | Get the set value from the tupellist.
searchTupel :: Integer -> TupelList -> BoolVal
searchTupel xval (xs : ys)
    | getVariableValue (fst xs) == xval = snd xs
    | not (null ys) = searchTupel xval ys
    | otherwise = BNothing  
    where var val = fst xs

searchTupel _ _ = BNothing  

-- | returns the clauseList from unitPropagation
getFirstElem :: TriTuple -> ClauseList
getFirstElem (x, _, _) = x

-- | returns the TupelList from unitPropagation
getSecondElem ::  TriTuple -> TupelList
getSecondElem (_, x, _) = x

-- | returns the MappedTupleList from unitPropagation
getThirdElem :: TriTuple -> MappedTupleList
getThirdElem (_, _, x) = x
