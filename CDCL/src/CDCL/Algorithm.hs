{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
----------------------------------------------------------------------- |
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


module CDCL.Algorithm (cdcl, interpret, searchTuple) where

import           CDCL.Decisionalalgorithm (getHighestActivity,
                     getShortestClauseViaActivity, halveActivityMap,
                     initialActivity, setVariableViaActivity, updateActivity)

import           CDCL.Types (Activity (..), ActivityMap, BoolVal (..),
                     CDCLResult (..), Clause, ClauseList, InterpretResult (..),
                     Level (..), MappedTupleList, Period (..), TriTuple, Tuple,
                     TupleClauseList, Variable (..), decreasePeriod,
                     getEmptyClause, getNOK, getVariableValue, increaseLvl, transformToLearnedClauses,
                     negateVariableValue, transformClauseList, getClauseFromReducedClauseAndOGClause,
                     getOGFromReducedClauseAndOGClause)
import qualified CDCL.Types as TypeC

import           CDCL.Unitpropagation (unitPropagation, unitResolution,
                     unitSubsumption)

import           CDCL.MapLogic (pushToMappedTupleList)

import           CDCL.Conflict (analyzeConflict)

import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe

hardCoded = Period 30
startBoundary = 20

-- | This function will start the CDCL Procedure.
--   To call this function do for example:
--   cdcl [[1,2,3],[2,5]]
--   cdcl [[1,2,3,4], [2,4], [4,5],[3,6,7],[3,9,1],[3,8,10]]
--   The function will return the result of the cdcl' function.
--   Function will immediately return SAT if ClauseList is null or UNSAT if an empty List is found within clist
cdcl :: [[Integer]] -> Bool -> CDCLResult
cdcl clist stats
    | checked = UNSAT
    | null clist && stats = SAT_WITH_STATS [] Map.empty 0 []
    | null clist = SAT []--[] Map.empty 0
    | otherwise = cdcl'
                  aMap
                  (Level 0)
                  []
                  Map.empty
                  transformedList
                  transformedList
                  []
                  []
                  transformedList
                  hardCoded
                  0
                  (startBoundary * 2)
                  startBoundary
                  stats
    where checked = any null clist
          transformedList = transformClauseList clist
          aMap = initialActivity transformedList Map.empty


-- | Function will first call the Unitpropagation Function.
--   Afterwards it will check if every Clause is interpreted.
--   If that isn't the case it will call functions related to
--   the Decision Algorithm.
--   In the case of getting NOK as result the function will call
--   the analyzeConflict Function to resolve the conflict.
--   After that the recursion starts again with Unitpropagation.
--   This happens until either SAT or UNSAT is returned as result.
cdcl'
  :: ActivityMap
  -> Level
  -> TupleClauseList
  -> MappedTupleList
  -> ClauseList
  -> ClauseList
  -> [Clause]
  -> [Clause]
  -> ClauseList
  -> Period
  -> Integer
  -> Integer
  -> Integer
  -> Bool
  -> CDCLResult
cdcl' aMap (Level lvl)  tlist mappedTL clistOG learnedClist learnedClauses confClauses clist period conflictIteration upperBound currentBoundary stats

    -- First and Second Case are part of Restart Algorithm with Luby Sequence
    -- current conflictiteration has same value like the current upper boundary. Restart the algorithm with higher upper boundary
    | conflictIteration == upperBound = cdcl' (initialActivity clistOG Map.empty)
                                              (Level 0)
                                              []
                                              Map.empty
                                              clistOG
                                              learnedClist
                                              learnedClauses
                                              confClauses
                                              learnedClist
                                              hardCoded
                                              0
                                              (upperBound*2)
                                              startBoundary
                                              stats

    -- current conflictiteration has same value like the current restart boundary. Restarts the algorithm with higher current boundary
    | conflictIteration == currentBoundary = cdcl' (initialActivity clistOG Map.empty)
                                                   (Level 0)
                                                   []
                                                   Map.empty
                                                   clistOG
                                                   learnedClist
                                                   learnedClauses
                                                   confClauses
                                                   learnedClist
                                                   hardCoded
                                                   0
                                                   upperBound
                                                   (currentBoundary * 2)
                                                   stats

    -- Interpret returned a NOK. Start the conflict analysis.
    | getNOK interpreted =
        let empty    = getEmptyClause interpreted
            analyzed = analyzeConflict (Level lvl) empty updatedMap halvedActivity
        in
          if getLevelFromAnalyze analyzed == Level (-1) then do
              if not stats then UNSAT else UNSAT_WITH_STATS (reverse (getClauseFromAnalyze analyzed :learnedClauses)) (reverse (empty :confClauses))
                else cdcl' (getActivityMapFromAnalyze analyzed)
                           (getLevelFromAnalyze analyzed)
                           (makeTupleClauseListFromAnalyze analyzed)
                           (getMappedTupleListFromAnalyze analyzed)
                           clistOG
                           ((getClauseFromAnalyze analyzed, getClauseFromAnalyze analyzed) : learnedClist) -- learnedClist
                           (getClauseFromAnalyze analyzed : learnedClauses) -- learnedClauses
                           (empty : confClauses) 
                           (calculateClauseList ((getClauseFromAnalyze analyzed, getClauseFromAnalyze analyzed) : learnedClist)
                           (makeTupleClauseListFromAnalyze analyzed))
                           periodUpdate2
                           (conflictIteration + 1)
                           upperBound
                           currentBoundary
                           stats

    -- Interpret retunred OK. Stop the algorithm.
    | interpreted == OK && stats = SAT_WITH_STATS (map fst tupleRes) updatedMap (toInteger (length learnedClist - length clistOG)) learnedClauses
    | interpreted == OK = SAT (map fst tupleRes) -- updatedMap (toInteger (length learnedClist - length clistOG))
    | otherwise = cdcl' halvedActivity
                        newLvl
                        list
                        updateMapViaDecision
                        clistOG
                        learnedClist
                        learnedClauses
                        confClauses
                        (calculateClauseList (getClauseListFromTriTuple res) list)
                        periodUpdate2
                        conflictIteration
                        upperBound
                        currentBoundary
                        stats
    where res = unitPropagation clist tlist (Level lvl) mappedTL
          tupleRes = getTupleClauseListFromTriTuple res
          updatedMap = getMappedTupleListFromTriTuple res
          interpreted = interpret learnedClist tupleRes

          periodUpdate = decreasePeriod period
          halvedActivity = if periodUpdate == Period 0 then halveActivityMap aMap (Map.keys aMap) else aMap
          periodUpdate2 = if periodUpdate == Period 0 then hardCoded else periodUpdate

          newLvl = increaseLvl (Level lvl)
          highestActivity = getHighestActivity (getClauseListFromTriTuple res) aMap [(Var 0, Activity (-1))]
          shortestCl = getShortestClauseViaActivity (getClauseListFromTriTuple res) [] highestActivity
          firstShortestCl = head shortestCl

          assuredShortestClause = getClauseFromReducedClauseAndOGClause firstShortestCl
          firstHighestActivityInClause = getHighestActivity [firstShortestCl] aMap [(Var 0, Activity (-1))]
          decided = setVariableViaActivity assuredShortestClause (head firstHighestActivityInClause) -- Need change here? it takes first highest found VariableActivity in the clause.
          updateMapViaDecision = uncurry (pushToMappedTupleList updatedMap newLvl) decided
          list = decided : tupleRes

-- | calculates the clauselist which will be given to unitpropagation.
--   returns when everything of tupelClauselist was calculated
calculateClauseList :: ClauseList -> TupleClauseList -> ClauseList
calculateClauseList cl tlist@(xs : ys)
    | null ys = reso
    | otherwise = calculateClauseList reso ys
    where sub = unitSubsumption cl (fst xs)
          reso = unitResolution sub (fst xs)
calculateClauseList cl [] = cl

-- | Interprets a given ClauseList based on a given TupleClauseList. Will call itself recursively
--   until a UNRESOLVED, NOK or OK is returned
--   Bsp: [[2,1,3],[-1]] [(1,0),(3,0),(2,0)] -> 0. CONFLICT
--   Bsp: [[2,1,3]][(1,0),(2,0)] -> -1. Etwas wurde noch nicht belegt o. etwas wurde nicht positiv.
interpret :: ClauseList -> TupleClauseList -> InterpretResult
interpret t@(formel : xs) interpretation

    -- Case: Clause found which cant be evaluated to 0 or 1
    | null interpretation || interpreted == UNRESOLVED = UNRESOLVED

    -- Case: Clause found which evalutes to 0.
    | getNOK interpreted = NOK (getOGFromReducedClauseAndOGClause formel)

    -- Case: None of the above cases appeared. Continue evaluating clauseList
    | not (null xs) = interpret xs interpretation

    -- Case: Returns OK as Result
    | otherwise = interpreted --interpret' (snd formel) interpretation False
    where interpreted = interpret' (getOGFromReducedClauseAndOGClause formel) interpretation False

-- | Interprets a single clause of a formula
--   It will return either
--   OK
--   NOK (emptyClause) <-- Clause which returns 0 with the set variables.
--   UNRESOLVED <-- No Variable evaluated the clause to 1.
interpret' :: Clause -> TupleClauseList -> Bool -> InterpretResult
interpret' (formel : xs) interpretation boolValue

    -- if calculated tupelValue isn't set and xs is null return UNRESOLVED
    | tupelValue == BNothing   && null xs = UNRESOLVED

    -- tupelValue not set but xs isn't null. Continue with the iteration.
    | tupelValue == BNothing  && not (null xs) = interpret' xs interpretation True

    -- The given Variable evalutes to 1. Returns OK
    | (formelValue >= 0 && tupelValue == BTrue) || (formelValue < 0 && tupelValue == BFalse) = OK

    -- Current Variable evalutes to 0 and xs is null. If it never entered to the second case it will return NOK. Otherwise it will enter below case to return UNRESOLVED
    | ((formelValue >= 0 && tupelValue == BFalse) || (formelValue < 0 && tupelValue == BTrue))  && null xs && not boolValue = NOK  (formel :  xs)

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

searchTuple _ [] = BNothing

-- | returns the clauseList from unitPropagation
getClauseListFromTriTuple :: TriTuple -> ClauseList
getClauseListFromTriTuple (x, _, _) = x

-- | returns the TupleClauseList from unitPropagation
getTupleClauseListFromTriTuple ::  TriTuple -> TupleClauseList
getTupleClauseListFromTriTuple (_, x, _) = x

-- | returns the MappedTupleList from unitPropagation
getMappedTupleListFromTriTuple :: TriTuple -> MappedTupleList
getMappedTupleListFromTriTuple (_, _, x) = x

-- | returns the new level after analyzing the conflict
getLevelFromAnalyze :: (Level, Clause, MappedTupleList, ActivityMap) -> Level
getLevelFromAnalyze (x, _, _, _) = x

-- | returns the new clauselist after analyzing the conflict
getClauseFromAnalyze :: (Level, Clause, MappedTupleList, ActivityMap) -> Clause
getClauseFromAnalyze (_, x, _, _) = x

-- | returns the new mappedtupleList after analyzing the conflict
getMappedTupleListFromAnalyze :: (Level, Clause, MappedTupleList, ActivityMap) -> MappedTupleList
getMappedTupleListFromAnalyze (_, _, x, _) = x

-- | function returns a tupleclauseList based on MappedTupleList from analyzing the conflict
makeTupleClauseListFromAnalyze :: (Level, Clause, MappedTupleList, ActivityMap) -> TupleClauseList
makeTupleClauseListFromAnalyze  = concat . getMappedTupleListFromAnalyze

-- | returns the new activityMap after analyzing the conflict
getActivityMapFromAnalyze :: (Level, Clause, MappedTupleList, ActivityMap) -> ActivityMap
getActivityMapFromAnalyze (_, _, _, x) = x
