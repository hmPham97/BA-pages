module Algorithm (unitPropagation, iterateList, interpret, searchTupel) where

-- [[Int]] ist Formel
-- | check first if unit clause exists
-- | 
unitPropagation :: [[Int]] -> [(Int, Int)] -> Int
unitPropagation formel interpretation = do
    let interpretedValue = interpret formel interpretation 
    if interpretedValue /= -1 then interpretedValue else 
        1 -- not finished


-- | iterate through [[Int]]. Return the shortest List of Integers
iterateList :: [[Int]] -> [Int]
iterateList a = []

-- | Returns 1 and -1
-- | 1 equals True, 0 equals False and -1 equals not resolved
interpret :: [[Int]] -> [(Int, Int)] -> Int
interpret (formel : xs) interpretation = do
    interpret' formel interpretation

-- | Returns 1 and -1
-- | Interprets a single clause of a formula
-- | This functions can't return 0 for an interpreted clause. If 0 gets returned it would mean the formula would be UNSAT in CNF.
interpret' :: [Int] -> [(Int, Int)] -> Int 
interpret' (formel : xs) interpretation = do
    let clauselValue = if formel < 0 then formel * (-1) else formel
    let tupelValue = searchTupel clauselValue interpretation
    let interpretValue  | tupelValue == -1 = -1 
                        | (formel > 0 && tupelValue == 1) || (formel < 0 && tupelValue == 0) = 1 
                        | otherwise = interpret' xs interpretation
    interpretValue
    

searchTupel :: Int -> [(Int, Int)] -> Int
searchTupel xval (xs : ys) = if fst xs == xval then snd xs else searchTupel xval ys
searchTupel xval _ = -1
