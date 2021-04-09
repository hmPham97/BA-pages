module Algorithm (interpret, dpll, searchTupel, cdcl, cdcl', calculateClauseList) where

import           Types (Clause, ClauseList, Level, Tupel, TupelList)
import           Unitpropagation (checkEmptyClause, checkSetVariable, unitProp,
                     unitResolution, unitSubsumption)
import           UPcdcl (unitPropagation)
-- | Returns 1 and -1 currently
-- | 1 equals resolved and -1 equals not resolved
dpll :: ClauseList -> TupelList -> Int
dpll d x = let clauseTupel = unitProp d x
    in if checkEmptyClause (fst clauseTupel) && not (checkTupleForError (snd clauseTupel)) then interpret d (snd clauseTupel) else -1

cdcl :: ClauseList-> TupelList -> Int
cdcl clist = cdcl' 0 clist clist -- Eta reduction. To call this do for example startCdcl [[1]] []

cdcl' :: Level -> ClauseList -> ClauseList -> TupelList -> Int
cdcl' lvl clistOG clist tlist = let res = unitPropagation clist tlist in
    if interpret clistOG (snd res) == 0 then -- checkEmptyClause needs to be changed. eventuell einfach interpret auf 0 checken?
        let empty = clist in -- function which calculates empty clause
            let analyzelv = lvl in -- function to analyze conflict
                -- backtrack-algorithm. will caculate new tlist or mapped tupleList
                cdcl' analyzelv clistOG clist tlist -- replace tlist with the backtrack algorithm
    else if interpret clistOG (snd res) == 1 then 1 else
        let newLvl = lvl + 1 in
            cdcl' newLvl clistOG (calculateClauseList (fst res) decided) decided
            where decided = [(2,1)] -- decided needs to become a proper function


calculateClauseList :: ClauseList -> TupelList -> ClauseList
calculateClauseList cl tlist@(xs : ys)
    | null xs = cl
    | null ys = reso
    | otherwise = calculateClauseList reso ys
    where sub = unitSubsumption cl xs
          reso = unitResolution sub xs

    --if checkEmptyClause clist then 1 else

-- | Bsp: [[2,1,3],[-1]] [(1,0),(3,0),(2,0)] -> 0. CONFLICT
-- | Bsp: [[2,1,3]][(1,0),(2,0)] -> -1. Etwas wurde noch nicht belegt o. etwas wurde nicht positiv.
interpret :: ClauseList -> TupelList -> Int
interpret t@(formel : xs) interpretation -- = do
    | null interpretation = -1
    | not (null xs) = if interpret' formel interpretation  False == 0 then 0 else interpret xs interpretation
    | otherwise = interpret' formel interpretation False

-- | Returns 1, 0 and -1
-- | Interprets a single clause of a formula
-- | Empty Clause if whole clause interprets to 0. if -1 appears it means the clause isnt finished from interpreting
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
    | fst xs == xval || fst xs * (-1) == xval = snd xs
    | not (null ys) = searchTupel xval ys
    | otherwise = -1
searchTupel xval x = if not (null x) then do
    if fst (head x) == xval then snd (head x) else -1
    else -1

-- | checks if tuple with -1 in snd is found
-- | return True if -1 is found in snd. else return false
checkTupleForError :: TupelList -> Bool
checkTupleForError (xs : ys)
    | snd xs == -1 = True
    | not (null ys) = checkTupleForError ys
    | otherwise = False
checkTupleForError x
    | snd (head x) == -1 = True
    | otherwise = False
