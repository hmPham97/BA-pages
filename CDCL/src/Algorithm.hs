module Algorithm (interpret, dpll, searchTupel) where

import           Types (Clause, ClauseList, Tupel, TupelList)
import           Unitpropagation (unitProp)

-- | Returns 1 and -1 currently
-- | 1 equals resolved and -1 equals not resolved
dpll :: ClauseList -> TupelList -> Int
dpll d x = let f = unitProp d x
    in interpret d f

interpret :: ClauseList -> TupelList -> Int
interpret t@(formel : xs) interpretation -- = do
    | not (null xs) = if interpret' formel interpretation == 0 then 0 else interpret xs interpretation
    | otherwise = interpret' formel interpretation

-- | Returns 1, 0 and -1
-- | Interprets a single clause of a formula
interpret' :: Clause -> TupelList -> Int
interpret' (formel : xs) interpretation -- = do
    | tupelValue == -1 = -1
    | (formel >= 0 && tupelValue == 1) || (formel < 0 && tupelValue == 0) = 1
    | ((formel >= 0 && tupelValue == 0) || (formel < 0 && tupelValue == 1))  && null xs = 0
    | otherwise = interpret' xs interpretation
        where clauselValue = if formel < 0 then formel * (-1) else formel
              tupelValue = searchTupel clauselValue interpretation


-- | Get the set value from the tupellist.
searchTupel :: Int -> TupelList -> Int
searchTupel xval (xs : ys)
    | fst xs == xval || fst xs * (-1) == xval = snd xs
    | not (null ys) = searchTupel xval ys
    | otherwise = -1
-- searchTupel xval x = if not (null x) then do
--     if fst (head x) == xval then snd (head x) else -1
--     else -1
