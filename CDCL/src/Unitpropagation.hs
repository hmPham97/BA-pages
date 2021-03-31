module Unitpropagation (unitProp) where

import           Types

unitProp :: ClauseList -> TupelList  -> TupelList
unitProp clauseList setTupel =
    if null clauseList then setTupel else do --setTupel
        let preCheck = getUnitClause clauseList
        if null preCheck then setTupel else do
            let calcTupel = setVariable preCheck
            if not (checkSetVariable setTupel (fst calcTupel)) then do
                let clauseCopy = unitSubsumption clauseList (fst calcTupel)
                let copy = unitResolution clauseCopy (fst calcTupel)
                unitProp copy (setTupel ++ [calcTupel]) else [(-1,-1)]

-- | checks if an unit clause exists in the given list of lists. if one exists return the list.
getUnitClause :: ClauseList  -> Clause
getUnitClause (clause : xs) = do
    let listLength = length clause
    if listLength == 1 then clause else getUnitClause xs

getUnitClause _ = []

-- | call this method on unit clauses only. If the value is less then 0 set a 0 in the tuple, else set 1
setVariable :: Clause  -> Tupel
setVariable clause = if head clause < 0 then (head clause, 0) else (head clause, 1)

-- | NOT CORRECTLY IMPLEMENTED
-- | if true -> variable is already set, else it isnt set
checkSetVariable :: TupelList  -> Int -> Bool
checkSetVariable (x:nxt) check = do
    let val = fst x
    val == check || val * (-1) == check|| not (null nxt) && checkSetVariable nxt check
checkSetVariable _ _ = False
    --  null x = False
    --  fst x * (-1) == check || fst x == check = True
    --  fst x * (-1) /= check || fst x /= check = not (null nxt) && checkSetVariable nxt check
    --  otherwise = False

-- | Remove clauses which have removableVar as variable.
unitSubsumption :: ClauseList  -> Int -> ClauseList
unitSubsumption (firstList : xs) removableVar = do
    let checked = checkInnerList firstList removableVar -- true if a set variable is found
    let list = if not checked then firstList : unitSubsumption xs removableVar else unitSubsumption xs removableVar
    filter (not . null) list

unitSubsumption _ _ = [[]]

-- | checks the list if the variable is inside the list
checkInnerList :: Clause -> Int -> Bool
checkInnerList list var = length (filter (== var) list) == 1

-- | remove -variable of the variable which was set
-- | cant remove variable if its the only one in list
unitResolution :: ClauseList -> Int -> ClauseList
unitResolution (firstList : xs) variable = do
    let checked = checkInnerList firstList (-variable)
    if not checked then firstList : unitResolution xs variable else do
        let list = if length firstList /= 1 then filter (/= -variable) firstList else firstList
        if not (null list) then list : unitResolution xs variable else unitResolution xs variable

unitResolution x l = filter (not . null) x

-- | possibly not needed since its not part of unitpropagation
checkConflict :: [(Int, Int)] -> [(Int,Int)]
checkConflict l = l
