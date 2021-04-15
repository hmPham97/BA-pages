module Decisionalgorithm (getShortestClause, initialActivity, updateActivity, act, getHighestActivity, getHighestActivity') where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe
import           Types (ActivityMap, Clause, ClauseList, Tupel,
                     VariableActivity)

filterK val = IntMap.filterWithKey (\x _ -> x == val)

decideVariable :: ClauseList -> ActivityMap -> Tupel
decideVariable cL = decideAlg (head cL)

decideAlg :: Clause -> ActivityMap -> Tupel
decideAlg cl aMap = (2,1)

getShortestClause :: ClauseList -> ClauseList -> [Clause]
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
act cList = initialActivity cList (IntMap.fromList [])

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
    | not (null activity) && null ys= IntMap.adjust (1 +) xs aMap
    | not (null activity) = let updatedMap = IntMap.adjust (1 +) xs aMap in
        updateActivity ys updatedMap
    -- --let actInt = (snd activity) + 1 in
    |  null activity = let updatedMap = IntMap.insert xs 1 aMap in
         updateActivity ys updatedMap
    | not (null ys) = updateActivity ys aMap
    where activity = filterK xs aMap

-- | example : getHighestActivity [[-1,3,5],[3,7]] (IntMap.fromList [(1,5),(3,6),(5,2),(7,7)]) 0
getHighestActivity :: ClauseList -> ActivityMap -> VariableActivity -> VariableActivity
getHighestActivity cList@(xs : ys) aMap val
    | snd val == 0 && null ys = highestValInClause
  --  | val == 0 && not (null ys) = getHighestActivity ys aMap highestValInClause
    | snd val < snd highestValInClause = getHighestActivity ys aMap highestValInClause
    where highestValInClause = getHighestActivity' xs aMap val
getHighestActivity _ _ val = val

-- | return the highest activity in a clause.
--   example getHighestActivity' [-1,3,5] (IntMap.fromList [(1,5),(3,6),(5,2)]) (0,0)
--   returns (3,6)
getHighestActivity' :: Clause -> ActivityMap -> VariableActivity -> VariableActivity
getHighestActivity' cl@(xs : ys) aMap val
    | actVal > Just (snd val) && null ys = (x, fromMaybe (fst val) actVal)
    | actVal > Just (snd val) = case actVal of
        Just act -> getHighestActivity' ys aMap (x, act)
    | otherwise = getHighestActivity' ys aMap val
    where x = if xs < 0 then (-xs) else xs
          activity = filterK x aMap
          actVal = activity IntMap.!? x
getHighestActivity' [] aMap x = x
