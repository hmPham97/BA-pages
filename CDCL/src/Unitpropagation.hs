module Unitpropagation (checkUnit, setVariable, unitSubsumption, checkInnerList, unitResolution) where

-- checks if an unit clause exists in the given list of lists. if one exists return the list.
checkUnit :: [[Int]] -> [Int] 
checkUnit (clause : xs) = do
    let listLength = length clause
    if listLength == 1 then clause else checkUnit xs

checkUnit _ = []

-- | call this method on unit clauses only. If the value is less then 0 set a 0 in the tuple, else set 1
setVariable :: [Int] -> (Int, Int)
setVariable clause = if head clause < 0 then (head clause, 0) else (head clause, 1)

unitSubsumption :: [[Int]] -> Int -> [[Int]]
unitSubsumption (firstList : xs) variable = do
    let checked = checkInnerList firstList variable -- true if a set variable is found 
    let list = if not checked then firstList : unitSubsumption xs variable else unitSubsumption xs variable
    filter (not . null) list


unitSubsumption _ _ = [[]]

-- | checks the list if the variable is inside the list
checkInnerList :: [Int] -> Int -> Bool
checkInnerList list var = length (filter (== var) list) == 1

-- | remove -variable of the variable which was set 
unitResolution :: [[Int]] -> Int -> [[Int]]
unitResolution (firstList : xs) variable = do
    let checked = checkInnerList firstList (-variable)
    if not checked then firstList : unitResolution xs variable else do 
        let list = filter (/= -variable) firstList
        list : unitResolution xs variable

unitResolution x l = filter (not . null) x