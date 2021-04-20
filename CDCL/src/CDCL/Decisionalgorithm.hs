---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Decisionalgorithm
-- Description :   Contains logic related to the decision algorithm
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :
-- Maintainer  :
-- Stability   :
-- Portability :
--
---------------------------------------------------------------------
module CDCL.Decisionalgorithm (getShortestClause, initialActivity, updateActivity, act,
    getHighestActivity, getHighestActivity', setVariableViaActivity,
    getShortestClauseViaActivity) where

import           CDCL.Types (ActivityMap, Clause, ClauseList, Tupel,
                     VariableActivity)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe

filterK val = Map.filterWithKey (\x _ -> x == val)

decideVariable :: ClauseList -> ActivityMap -> Tupel
decideVariable cL = decideAlg (head cL)

decideAlg :: Clause -> ActivityMap -> Tupel
decideAlg cl aMap = (2,1)

getShortestClause :: ClauseList -> ClauseList -> ClauseList
getShortestClause (xs : ys) cur
    | null xs && not (null cur) = cur
    | null curSize && not (null xs) && not (null ys)= getShortestClause ys [xs]
    | null curSize = [xs]
    | length xs == curLen && null ys = xs : cur
    | length xs > curLen && null ys= cur
    | otherwise = filter ((< length xs) .  length) (getShortestClause ys (xs : cur))
    where curSize = filter ((<= length xs) . length) cur
          curLen = length (head curSize)
getShortestClause [] cur = cur


act :: ClauseList -> ActivityMap
act cList = initialActivity cList Map.empty

-- | calculate the ActivityMap.
--   example: initialActivity [[1,2,3,4],[3,4]] (IntMap.fromList [])
--   result: fromList [(1,1),(2,1),(3,2),(4,2)]
initialActivity :: ClauseList -> ActivityMap -> ActivityMap
initialActivity cList@(xs : ys) aList
    | not (null ys) = let updatedMap = initialActivity ys aList in
        updateActivity xs updatedMap
    | otherwise = updateActivity xs aList
initialActivity x aList = updateActivity (head x) aList

-- | updates the activitymap.
--   example : updateActivity [1,2] (IntMap.fromList [(1,1),(2,1),(3,2),(4,2)])
--   result : fromList [(1,2),(2,2),(3,2),(4,2)]
updateActivity :: Clause -> ActivityMap -> ActivityMap
updateActivity [] aMap = aMap
updateActivity clause@(xs : ys) aMap
    | not (null activity) && null ys= Map.adjust (1 +) xValue aMap
    | not (null activity) = let updatedMap = Map.adjust (1 +) xValue aMap in
        updateActivity ys updatedMap
    -- --let actInt = (snd activity) + 1 in
    |  null activity = let updatedMap = Map.insert xValue 1 aMap in
         updateActivity ys updatedMap
    | not (null ys) = updateActivity ys aMap
    where xValue = if xs < 0 then (-xs) else xs
          activity = filterK xValue aMap

-- | example : getHighestActivity [[-1,3,5],[3,7]] (Map.fromList [(1,5),(3,6),(5,2),(7,7)]) (0,0)
getHighestActivity :: ClauseList -> ActivityMap -> VariableActivity -> VariableActivity
getHighestActivity cList@(xs : ys) aMap val
    | snd val == 0 && null ys = highestValInClause
  --  | val == 0 && not (null ys) = getHighestActivity ys aMap highestValInClause
    | snd val < snd highestValInClause = getHighestActivity ys aMap highestValInClause
    where highestValInClause = getHighestActivity' xs aMap val
getHighestActivity _ _ val = val

-- | return the highest activity in a clause.
--   example getHighestActivity' [-1,3,5] (Map.fromList [(1,5),(3,6),(5,2)]) (0,0)
-- getHighestActivity' [-1,2] (Map.fromList [(1,1),(2,1)]) (0,0)
--   returns (3,6)
getHighestActivity' :: Clause -> ActivityMap -> VariableActivity -> VariableActivity
getHighestActivity' cl@(xs : ys) aMap val
    | actVal > Just (snd val) && null ys = (x, fromMaybe (fst val) actVal)
    | actVal > Just (snd val) = case actVal of
        Just act -> getHighestActivity' ys aMap (x, act)
    | otherwise = getHighestActivity' ys aMap val
    where x = if xs < 0 then (-xs) else xs
          activity = filterK x aMap
          actVal = activity Map.!? x
getHighestActivity' [] aMap x = x

setVariableViaActivity :: Clause -> VariableActivity -> Tupel
setVariableViaActivity [] _ = (-1,-1)
setVariableViaActivity (xs : ys) vAct
    | xs == fst vAct = if xs < 0 then (-xs, 1) else (xs, 0)
    | not (null ys) = setVariableViaActivity ys vAct

getShortestClauseViaActivity :: ClauseList -> VariableActivity -> Maybe Clause
getShortestClauseViaActivity (xs : ys) vAct
    | fst vAct `elem` xs  = Just xs
    | otherwise = getShortestClauseViaActivity ys vAct
getShortestClauseViaActivity [] vAct = Nothing
