{-|
Module      :   Algorithm
Description :   General algorithms for DPLL and CDCL Algorithm.
                Includes interpreting clauses, clauseLists,
                tuple values and calculating ClauseList.
Copyright   :   (c) Thanh Nam Pham, 2021
License     :   
Maintainer  :   
Stability   :   
Portability :   
-}

module Algorithm (interpret, dpll, searchTupel, cdcl, cdcl', calculateClauseList) where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Decisionalgorithm (getShortestClause, initialActivity,
                     updateActivity)
import           Types (ActivityMap, Clause, ClauseList, Level, Tupel,
                     TupelList)
import           Unitpropagation (checkSetVariable, unitResolution,
                     unitSubsumption)
import           UPcdcl (unitPropagation)
-- | Returns 1 and -1 currently
--   1 equals resolved and -1 equals not resolved
dpll :: ClauseList -> TupelList -> Int
dpll d x = let clauseTupel = unitPropagation d x
    in if interpret d (snd clauseTupel) /= 1 then (-1) else 1
   -- in if checkEmptyClause (fst clauseTupel) && not (checkTupleForError (snd clauseTupel)) then interpret d (snd clauseTupel) else -1

cdcl :: ActivityMap -> ClauseList-> TupelList -> Int
cdcl aMap clist = let aMap' = initialActivity clist (IntMap.fromList []) in
    cdcl' aMap' 1 clist clist -- Eta reduction. To call this do for example Cdcl [[1]] []

-- | Implementation not done
cdcl' :: ActivityMap -> Level -> ClauseList -> ClauseList -> TupelList -> Int
cdcl' aMap lvl clistOG clist tlist
    | interpreted == 0 = let empty = clist in
        let analyzelv = lvl in
            error "not implemented"
            --cdcl' analyzelv clistOG clist tlist
    | interpreted == 1 = 1
    | lvl > 4 = error "stop"
    | otherwise = let newLvl = lvl + 1 in
        cdcl' aMap newLvl clistOG (calculateClauseList (fst res) decided) decided
    where res = unitPropagation clist tlist
          interpreted = interpret clistOG (snd res)
          shortestClause = getShortestClause clist
          decided = [(2,1)]

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
interpret :: ClauseList -> TupelList -> Int
interpret t@(formel : xs) interpretation -- = do
    | null interpretation = -1
    | not (null xs) = let value = interpret' formel interpretation False in
        if value /= 1 then value else interpret xs interpretation
    | otherwise = interpret' formel interpretation False

-- | Returns 1, 0 and -1
--   Interprets a single clause of a formula
--   Empty Clause if whole clause interprets to 0. if -1 appears it means the clause isnt finished from interpreting
interpret' :: Clause -> TupelList -> Bool -> Int
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
searchTupel :: Int -> TupelList -> Int
searchTupel xval (xs : ys)
    | fst xs == xval = snd xs
    | not (null ys) = searchTupel xval ys
    | otherwise = -1
searchTupel _ _ = -1
-- searchTupel xval x = if not (null x) then do
--     if fst (head x) == xval then snd (head x) else -1
--     else -1

