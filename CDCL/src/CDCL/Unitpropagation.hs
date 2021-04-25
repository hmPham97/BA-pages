---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Unitpropagation
-- Description :   Contains necessary logic for unitpropagation
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :
-- Maintainer  :
-- Stability   :
-- Portability :
--
---------------------------------------------------------------------
module CDCL.Unitpropagation (getUnitClause, setVariable, unitSubsumption, checkSetVariable, unitResolution, pushToMappedTupleList) where

import           CDCL.Types (Clause, ClauseList, Level, MappedTupleList, Tupel,
                     TupelList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe

-- | checks if an unit clause exists in the given list of lists. if one exists return the list.
getUnitClause :: ClauseList  -> Clause
getUnitClause (clause : xs) = let listLength = length clause in
    if listLength == 1 then clause else getUnitClause xs

getUnitClause _ = []

-- | call this method on unit clauses only. If the value is less then 0 set a 0 in the tuple, else set 1
setVariable :: Clause  -> Tupel
setVariable clause = if head clause < 0 then (-(head clause), 0) else (head clause, 1) -- Need change here

-- | Method for updating MappedTupleList.
--   If Variable was already set return map.
--   If Variable was not set and lvl has no list -> insert the TupelList
--   If Variable was not set but lvl has already a list -> update the TupelList
pushToMappedTupleList :: MappedTupleList -> Level -> Tupel -> MappedTupleList
pushToMappedTupleList maptl lvl tupel
    | Data.Maybe.isJust f && null check = Map.update m lvl maptl
    | Data.Maybe.isNothing f = Map.insert lvl [tupel] maptl
    | otherwise = maptl
    where f = Map.lookup lvl maptl
          check = filter ((== fst tupel) . fst) (fromMaybe [] f)
          m x = Just (fromMaybe [] f ++ [tupel])

-- | if true -> variable is already set, else it isnt set
checkSetVariable :: TupelList  -> Integer -> Bool
checkSetVariable (x:nxt) check = let val = fst x in
    val == check || (not (null nxt) && checkSetVariable nxt check)
checkSetVariable _ _ = False

-- | Remove clauses which have removableVar as variable.
unitSubsumption :: ClauseList  -> Tupel -> ClauseList
unitSubsumption (firstList : xs) tuple
    | not checked = filter (not . null) (firstList : unitSubsumption xs tuple)
    | otherwise = filter (not . null) (unitSubsumption xs tuple)
    where val = if snd tuple == 1 then fst tuple else -(fst tuple)
          checked = val `elem` firstList -- checks if val is inside list

unitSubsumption _ _ = [[]]

-- | remove -variable of the variable which was set
unitResolution :: ClauseList -> Tupel  -> ClauseList
unitResolution (firstList : xs) tuple
    | not checked = filter (not . null) (firstList : unitResolution xs tuple)
    | otherwise = let list = filter (/= val) firstList in
        filter (not . null) (list : unitResolution xs tuple)
    where val = if snd tuple == 0 then fst tuple else (-(fst tuple))
          checked = val `elem` firstList -- checks if val is inside list

unitResolution _ _ = [[]]
