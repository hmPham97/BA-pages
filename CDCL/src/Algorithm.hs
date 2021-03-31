module Algorithm (interpret, dpll) where

import Unitpropagation
import Types

-- | Returns 1 and -1 currently
-- | 1 equals resolved and -1 equals not resolved
dpll :: ClauseList -> TupelList -> Int
dpll d x = do 
    let f = unitProp d x 
    interpret d f

interpret :: ClauseList -> TupelList -> Int
interpret t@(formel : xs) interpretation = do
    --let f = unitProp t interpretation
    if not (null xs) then do
        if interpret' formel interpretation == 0 then 0 else interpret xs interpretation
        else interpret' formel interpretation 

    -- if interpret' formel interpretation /= 1 then 0 else if not (null xs) then interpret xs interpretation
    -- else 

-- | Returns 1, 0 and -1
-- | Interprets a single clause of a formula
interpret' :: Clause -> TupelList -> Int 
interpret' (formel : xs) interpretation = do
    let clauselValue = if formel < 0 then formel * (-1) else formel
    let tupelValue = searchTupel clauselValue interpretation
    let interpretValue  | tupelValue == -1 = -1 
                        | (formel > 0 && tupelValue == 1) || (formel < 0 && tupelValue == 0) = 1 
                        | (formel > 0 && tupelValue == 0) || (formel < 0 && tupelValue == 1) = 0
                        | otherwise = interpret' xs interpretation
    interpretValue
    

-- | Get the set value from the tupellist.
searchTupel :: Int -> TupelList -> Int
searchTupel xval (xs : ys) 
    | fst xs == xval || fst xs * (-1) == xval = snd xs
    | not (null ys) = searchTupel xval ys 
    | otherwise = -1
searchTupel xval x = if not (null x) then do
    if fst (head x) == xval then snd (head x) else -1 
    else -1
