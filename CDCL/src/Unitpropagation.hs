module Unitpropagation (unitProp, checkEmptyClause, getUnitClause, setVariable, unitSubsumption, checkSetVariable, unitResolution, checkInnerList) where

import           Types (Clause, ClauseList, Tupel, TupelList)

unitProp :: ClauseList -> TupelList -> (ClauseList, TupelList)
unitProp clauseList setTupel
  | null clauseList  = (clauseList,setTupel)
  | null preCheck    = (clauseList,setTupel)
  | check1 /= check2 = (clauseList,[(-1,-1)])
  | not checkSetV    = unitProp copy (setTupel ++ [calcTupel])
  | otherwise        = (clauseList,[(-1,-1)])
    where preCheck   = getUnitClause clauseList
          calcTupel  = setVariable preCheck
          fstTuple   = fst calcTupel
          clauseCopy = unitSubsumption clauseList calcTupel
          check1     = checkEmptyClause clauseCopy
          copy       = unitResolution clauseCopy calcTupel
          check2     = checkEmptyClause copy
          checkSetV  = checkSetVariable setTupel fstTuple


-- | checks if an unit clause exists in the given list of lists. if one exists return the list.
getUnitClause :: ClauseList  -> Clause
getUnitClause (clause : xs) = let listLength = length clause in
    if listLength == 1 then clause else getUnitClause xs

getUnitClause _ = []

-- | call this method on unit clauses only. If the value is less then 0 set a 0 in the tuple, else set 1
setVariable :: Clause  -> Tupel
setVariable clause = if head clause < 0 then (-(head clause), 0) else (head clause, 1)

-- | NOT CORRECTLY IMPLEMENTED
-- | if true -> variable is already set, else it isnt set
checkSetVariable :: TupelList  -> Int -> Bool
checkSetVariable (x:nxt) check = let val = fst x in
    val == check || val * (-1) == check|| not (null nxt) && checkSetVariable nxt check
checkSetVariable _ _ = False

-- | Remove clauses which have removableVar as variable.
unitSubsumption :: ClauseList  -> Tupel -> ClauseList
unitSubsumption (firstList : xs) tuple
    | not checked = filter (not . null) (firstList : unitSubsumption xs tuple)
    | otherwise = filter (not . null) (unitSubsumption xs tuple)
    where val = if snd tuple == 1 then fst tuple else -(fst tuple)
          checked = checkInnerList firstList val

unitSubsumption _ _ = [[]]


-- | checks the list if the variable is inside the list
checkInnerList :: Clause -> Int -> Bool
checkInnerList list var
    | length (filter (== var) list) == 1 = True
    | otherwise = False

-- | remove -variable of the variable which was set
-- | cant remove variable if its the only one in list
unitResolution :: ClauseList -> Tupel  -> ClauseList
unitResolution (firstList : xs) tuple
    | not checked = firstList : unitResolution xs tuple
    | otherwise = let list = filter (/= -val) firstList in
            list : unitResolution xs tuple
    where val = if snd tuple == 0 then (- (fst tuple)) else fst tuple
          checked = checkInnerList firstList val

unitResolution x l = x --filter (not . null) x

-- | possibly not needed since its not part of unitpropagation
checkConflict :: [(Int, Int)] -> [(Int,Int)]
checkConflict l = l

-- | this is implemented wrong. can use interpret to check if empty clause.
-- | muss mithilfe von tupel schauen ob empty clause gefunden wird.
-- | bsp: [[1]] [(1,0)] -> Empty Clause
checkEmptyClause :: ClauseList -> Bool
checkEmptyClause (xs : ys)
    | null xs = True
    | not (null ys) = checkEmptyClause ys
    | otherwise = False
checkEmptyClause xs
    | null xs = True
    | otherwise = False
