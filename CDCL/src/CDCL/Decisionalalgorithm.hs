---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Decisionalgorithm
-- Description :   Contains logic related to the decision algorithm
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :   Apache-2.0
-- Maintainer  :
-- Stability   :
-- Portability :
--
---------------------------------------------------------------------
module CDCL.Decisionalalgorithm (initialActivity, updateActivity, halveActivityMap,
    getHighestActivity, setVariableViaActivity, getShortestClauseViaActivity) where

import           CDCL.Types (Activity (..), ActivityMap, BoolVal (..), Clause,
                     ClauseList, Reason (..), Tuple, TupleClause,
                     Variable (..), VariableActivity, divideActivity,
                     getActivityValue, getVariableValue, increaseActivity,
                     negateVariableValue)
import qualified CDCL.Types as TypeC
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe

filterK val = Map.filterWithKey (\x _ -> x == val)


-- | calculate the ActivityMap. calls itself recursively until every clause
--   is calculated. Returns a filled ActivityMap.
--   example: initialActivity [[1,2,3,4],[3,4]] (IntMap.fromList [])
--   result: fromList [(1,1),(2,1),(3,2),(4,2)]
initialActivity :: ClauseList -> ActivityMap -> ActivityMap
initialActivity cList@(xs : ys) aList
    | not (null ys) = initialActivity ys updated
    | otherwise = updated
    where updated = updateActivity (fst xs) aList
-- initialActivity x aList = updateActivity (fst (head x)) aList

-- | updates the activitymap.
--   example : updateActivity [1,2] (IntMap.fromList [(1,1),(2,1),(3,2),(4,2)])
--   result : fromList [(1,2),(2,2),(3,2),(4,2)]
updateActivity :: Clause -> ActivityMap -> ActivityMap
updateActivity [] aMap = aMap
updateActivity clause@(xs : ys) aMap
    | not (null activityMap) = let updatedMap = Map.adjust increaseActivity xValue aMap in
        updateActivity ys updatedMap
    -- --let actInt = (snd activity) + 1 in
    |  null activityMap = let updatedMap = Map.insert xValue (Activity 1) aMap in
         updateActivity ys updatedMap
    | not (null ys) = updateActivity ys aMap
    where xValue = if getVariableValue xs < 0 then negateVariableValue xs else xs
          activityMap = filterK xValue aMap
          activity = Map.lookup xValue activityMap
          actval = fromMaybe (Activity (-1)) activity

-- | periodically call this function to half the activities in the map.
halveActivityMap :: ActivityMap -> [Variable] -> ActivityMap
halveActivityMap = foldr (Map.adjust divideActivity)
-- above code does following thing:
-- halveActivityMap aMap (xs : ys) = let updateMap = Map.adjust divideActivity xs aMap
--     in halveActivityMap updateMap ys
-- halveActivityMap aMap [] = aMap

-- | Return the highest Activity which can be found in the ClauseList. Calls itself recursively
--   until every clauses was calculated.
--   example : getHighestActivity [([Variable (-1), Variable 3, Variable 5],[Variable (-1), Variable 3, Variable 5]) ,
--   ([Variable 3, Variable 7],[Variable 3, Variable 7])]
--   (Map.fromList [(Variable 1, Activity 5),(Variable 3, Activity 6),(Variable 5,Activity 2),(Variable 7,Activity 7)]) (Variable 0, Activity 0)
getHighestActivity :: ClauseList -> ActivityMap -> [VariableActivity] -> [VariableActivity]
getHighestActivity cList@(xs : ys) aMap val
    | getActivityValue (snd firstVal) < getActivityValue (snd foundAct) = getHighestActivity ys aMap highestValInClause
    | getActivityValue (snd firstVal) > getActivityValue (snd foundAct) = getHighestActivity ys aMap val
    | getActivityValue (snd firstVal) == getActivityValue (snd foundAct) = getHighestActivity ys aMap nubList
    where firstVal = head val
          highestValInClause = getHighestActivity' (fst xs) aMap val
          firstActVal = getActivityValue (snd firstVal)
          foundAct = head highestValInClause
          nubList = nub (val ++ highestValInClause)

getHighestActivity _ _ val = val

-- | return the highest activity in a clause.
--   example getHighestActivity' [-1,3,5] (Map.fromList [(1,5),(3,6),(5,2)]) (0,0)
--   getHighestActivity' [-1,2] (Map.fromList [(1,1),(2,1)]) (0,0)
--   returns (3,6)
getHighestActivity' :: Clause -> ActivityMap -> [VariableActivity] -> [VariableActivity]
getHighestActivity' cl@(xs : ys) aMap val
    | actVal > snd firstVal = getHighestActivity' ys aMap [(x, actVal)]
    | actVal == snd firstVal = getHighestActivity' ys aMap (val ++ [(x, actVal)])
    | otherwise = getHighestActivity' ys aMap val
    where firstVal = head val
          x = if getVariableValue xs < 0 then negateVariableValue xs else xs
          activity = filterK x aMap
          actVal = fromMaybe (Activity 0) (activity Map.!? x)
          nubList = nub (val ++ [(x, actVal)])
getHighestActivity' [] aMap x = x

-- | Set the Tupelvalue based on the Variable.
--   If the Variable with the highest activity has a minus prefix the tupel value will
--   be set to 1 with the variable getting a positive prefix in the tupel.
--   Else the tupel will be set to the variable with a 0 as second value.
setVariableViaActivity :: Clause -> VariableActivity -> TupleClause
setVariableViaActivity (xs : ys) vAct
    | xs == fst vAct = ((xs, BFalse), Decision)
    | negateVariableValue xs == fst vAct = ((negateVariableValue xs, BTrue), Decision)
    | otherwise = setVariableViaActivity ys vAct
    where varValue = getVariableValue xs
setVariableViaActivity [] vAct = error "wrong input in VariableActivity or Clause"--((Variable (-1), BNothing), Reason [Variable (-1)])

-- | Get the shortest clause which contains the highest activity.
--   Do this based on the given ClauseList and VariableActivity. Returns
--   a ClauseList
getShortestClauseViaActivity :: ClauseList -> ClauseList -> [VariableActivity] -> ClauseList
getShortestClauseViaActivity (xs : ys) checkC vAct
    | null checkC && checkClause = getShortestClauseViaActivity ys [xs] vAct
    | not checkClause || xsLen > headLen = getShortestClauseViaActivity ys checkC vAct
    | null filterClause && xsLen < headLen  = [xs]
    | xsLen == headLen = getShortestClauseViaActivity ys  (checkC ++ [xs]) vAct
    | xsLen < headLen = getShortestClauseViaActivity filterClause [xs] vAct
    where checkClause = checkClauseForVariable (fst xs) vAct
          xsLen = length (fst xs)
          headLen = length (fst (head checkC))
          filterClause = filter (\x -> length (fst x) <= length (fst xs)) ys

    --  firstVal `elem` fst xs || negateVariableValue firstVal `elem` fst xs = Just (fst xs)
    --  otherwise = getShortestClauseViaActivity ys vAct
    --where firstVal = fst vAct
getShortestClauseViaActivity [] checkC _ = checkC

-- | Checks if the given Clause contains a given VariableActivity. Returns true if it does
--   else return false.
checkClauseForVariable :: Clause -> [VariableActivity] -> Bool
checkClauseForVariable cl (x : ys)
    | fst x `elem` cl || negateVariableValue (fst x) `elem` cl = True
    | otherwise = checkClauseForVariable cl ys
checkClauseForVariable cl [] = False
