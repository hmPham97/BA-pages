---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Algorithm
-- Description :   General algorithms for DPLL and CDCL Algorithm.
--                 Includes interpreting clauses, clauseLists,
--                 tuple values and calculating ClauseList.
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :
-- Maintainer  :
-- Stability   :
-- Portability :
--
---------------------------------------------------------------------


module CDCL.Algorithm (interpret, dpll, searchTupel, cdcl, cdcl', calculateClauseList) where

import           CDCL.Decisionalgorithm (getHighestActivity,
                     getHighestActivity', getShortestClause,
                     getShortestClauseViaActivity, initialActivity,
                     setVariableViaActivity, updateActivity)
import           CDCL.Types (ActivityMap, CDCLResult (..), Clause, ClauseList,
                     DPLLResult (..), Level, MappedTupleList, Tupel, TupelList, TriTuple)
import           CDCL.Unitpropagation (checkSetVariable, pushToMappedTupleList,
                     unitResolution, unitSubsumption)
import           CDCL.UPcdcl (unitPropagation)
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
-- | Returns 1 and -1 currently
--      1 equals resolved and -1 equals not resolved
dpll :: ClauseList -> TupelList -> DPLLResult
dpll d x = let clauseTupel = unitPropagation d x 1 Map.empty
    in if interpret d (getSecondElem clauseTupel) /= 1 then DNotResolved else DResolved
   -- in if checkEmptyClause (fst clauseTupel) && not (checkTupleForError (snd clauseTupel)) then interpret d (snd clauseTupel) else -1

-- | This function will start the CDCL Function.
--   To call this function do for example:
--   cdcl [[1,2,3],[2,5]]
--   The function will return the result of the cdcl' function.
cdcl :: ClauseList -> CDCLResult
cdcl clist = let aMap = initialActivity clist Map.empty in
    cdcl' aMap 1 [] Map.empty clist clist -- Eta reduction. To call this do for example Cdcl [[1]] []

-- | Implementation not done
--   Function recursively calls itself until either following result happens
--   interpreted = 1 -> SAT TupelList
--   interpreted = 0 and lvl = 0 -> UNSAT
cdcl' :: ActivityMap -> Level -> TupelList -> MappedTupleList -> ClauseList -> ClauseList -> CDCLResult
cdcl' aMap lvl tlist mappedTL clistOG clist
    | interpreted == 0 = let empty = clist in
        let analyzelv = lvl in
            error "not implemented"
            --cdcl' analyzelv clistOG clist tlist
    | interpreted == 1 = SAT tupleRes updatedMap
    | lvl > 10 = error "stop"
    | otherwise = cdcl' aMap newLvl list updateMapViaDecision clistOG (calculateClauseList (getFirstElem res) list)
    where res = unitPropagation clist tlist lvl mappedTL
          tupleRes = getSecondElem res
          updatedMap = getThirdElem res
          interpreted = interpret clistOG tupleRes
          newLvl = lvl + 1
          shortestClauses = getShortestClause (getFirstElem res) []
          highestActivity = getHighestActivity shortestClauses aMap (0,0)
          shortestCl = getShortestClauseViaActivity shortestClauses highestActivity
          decided = setVariableViaActivity (fromMaybe [] shortestCl) highestActivity -- Need change here
          updateMapViaDecision = pushToMappedTupleList mappedTL lvl decided
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
    | tupelValue == -1 && null xs = -1
    | tupelValue == -1 && not (null xs) = interpret' xs interpretation True
    | (formel >= 0 && tupelValue == 1) || (formel < 0 && tupelValue == 0) = 1
    | ((formel >= 0 && tupelValue == 0) || (formel < 0 && tupelValue == 1))  && null xs && not x= 0
    | x && null xs = -1
    | otherwise = interpret' xs interpretation x
        where clauselValue = if formel < 0 then formel * (-1) else formel
              tupelValue = searchTupel clauselValue interpretation


-- | Get the set value from the tupellist.
searchTupel :: Integer -> TupelList -> Integer
searchTupel xval (xs : ys)
    | fst xs == xval = snd xs
    | not (null ys) = searchTupel xval ys
    | otherwise = -1
searchTupel _ _ = -1

-- | returns the clauseList from unitPropagation
getFirstElem :: TriTuple -> ClauseList
getFirstElem (x, _, _) = x

-- | returns the TupelList from unitPropagation
getSecondElem ::  TriTuple -> TupelList
getSecondElem (_, x, _) = x

-- | returns the MappedTupleList from unitPropagation
getThirdElem :: TriTuple -> MappedTupleList
getThirdElem (_, _, x) = x
