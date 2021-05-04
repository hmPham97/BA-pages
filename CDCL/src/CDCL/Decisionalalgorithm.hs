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
module CDCL.Decisionalalgorithm (getShortestClause, initialActivity, updateActivity,
    getHighestActivity, getHighestActivity', setVariableViaActivity,
    getShortestClauseViaActivity) where

import           CDCL.Types (ActivityMap, Clause, ClauseList, Tupel, TupelClause,
                     VariableActivity, Variable(..), Activity(..), BoolVal(..), Reason (..),
                     getVariableValue, negateVariableValue, getActivityValue, increaseActivity)
import qualified CDCL.Types as TypeC
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe

filterK val = Map.filterWithKey (\x _ -> x == val)

-- | Get the shortest clauses within the given clauseList.
--   Returns either empty list or a clauseList with equally long clauses.
--   E.g. getShortestClause [[3,33],[4,5,4],[3,4],[52,12]] []
--   will return [[3,33],[3,4],[52,12]]
getShortestClause :: ClauseList -> ClauseList -> ClauseList
getShortestClause (xs : ys) cur
    | null curSize && not (null xs) = getShortestClause shorterYs [xs]
    | length xs == curLen = getShortestClause shorterYs (cur ++ [xs])
    | length xs > curLen = getShortestClause shorterYs cur
    --  otherwise = filter ((< length xs) .  length) (getShortestClause ys (xs : cur)) Fall sollte nicht mehr notwendig sein
    where curSize = filter ((<= length xs) . length) cur
          curLen = length (head curSize)
          shorterYs = filter ((<= length xs) . length) ys
getShortestClause [] cur = cur

-- | calculate the ActivityMap. calls itself recursively until every clause
--   is calculated. Returns a filled ActivityMap.
--   example: initialActivity [[1,2,3,4],[3,4]] (IntMap.fromList [])
--   result: fromList [(1,1),(2,1),(3,2),(4,2)]
initialActivity :: ClauseList -> ActivityMap -> ActivityMap
initialActivity cList@(xs : ys) aList
    | not (null ys) = let updatedMap = initialActivity ys aList in
        updateActivity (fst xs) updatedMap
    | otherwise = updateActivity (fst xs) aList
initialActivity x aList = updateActivity (fst (head x)) aList

-- | updates the activitymap.
--   example : updateActivity [1,2] (IntMap.fromList [(1,1),(2,1),(3,2),(4,2)])
--   result : fromList [(1,2),(2,2),(3,2),(4,2)]
updateActivity :: Clause -> ActivityMap -> ActivityMap
updateActivity [] aMap = aMap
updateActivity clause@(xs : ys) aMap
    | not (null activityMap) && null ys= Map.adjust increaseActivity xValue aMap
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

-- | Return the highest Activity which can be found in the ClauseList. Calls itself recursively
--   until every clauses was calculated.
--   example : getHighestActivity [[-1,3,5],[3,7]] (Map.fromList [(1,5),(3,6),(5,2),(7,7)]) (0,0)
getHighestActivity :: ClauseList -> ActivityMap -> VariableActivity -> VariableActivity
getHighestActivity cList@(xs : ys) aMap val
    | getActivityValue (snd val) == 0 && null ys = highestValInClause
  --  | val == 0 && not (null ys) = getHighestActivity ys aMap highestValInClause
    | getActivityValue (snd val) < getActivityValue (snd highestValInClause) = getHighestActivity ys aMap highestValInClause
    where highestValInClause = getHighestActivity' (fst xs) aMap val
getHighestActivity _ _ val = val

-- | return the highest activity in a clause.
--   example getHighestActivity' [-1,3,5] (Map.fromList [(1,5),(3,6),(5,2)]) (0,0)
--   getHighestActivity' [-1,2] (Map.fromList [(1,1),(2,1)]) (0,0)
--   returns (3,6)
getHighestActivity' :: Clause -> ActivityMap -> VariableActivity -> VariableActivity
getHighestActivity' cl@(xs : ys) aMap val
    | actVal > Just (snd val) && null ys = (x, fromMaybe (snd val) actVal)
    | actVal > Just (snd val) = case actVal of
        Just act -> getHighestActivity' ys aMap (x, act)
    | otherwise = getHighestActivity' ys aMap val
    where x = if getVariableValue xs < 0 then negateVariableValue xs else xs
          activity = filterK x aMap
          actVal = activity Map.!? x
getHighestActivity' [] aMap x = x

-- | Set the Tupelvalue based on the Variable.
--   If the Variable with the highest activity has a minus prefix the tupel value will
--   be set to 1 with the variable getting a positive prefix in the tupel.
--   Else the tupel will be set to the variable with a 0 as second value.
setVariableViaActivity :: Clause -> VariableActivity -> TupelClause
setVariableViaActivity (xs : ys) vAct
    | xs == fst vAct || (-getVariableValue xs) == getVariableValue (fst vAct) = if varValue < 0 then ((negateVariableValue xs, BTrue), Decision) else ((xs, BFalse), Decision)
    | not (null ys) = setVariableViaActivity (ys ++ [xs])  vAct
    where varValue = getVariableValue xs
setVariableViaActivity [] _ = ((Variable (-1), BNothing), Reason [Variable (-1)])

-- | Get the shortest clause which contains the highest activity.
--   Do this based on the give ClauseList and VariableActivity. Return
--   Maybe Clause or Nothing.
getShortestClauseViaActivity :: ClauseList -> VariableActivity -> Maybe Clause
getShortestClauseViaActivity (xs : ys) vAct
    | firstVal `elem` fst xs || negateVariableValue firstVal `elem` fst xs = Just (fst xs)
    | otherwise = getShortestClauseViaActivity ys vAct
    where firstVal = fst vAct
getShortestClauseViaActivity [] vAct = Nothing
