module CDCL.CDCLFilereader (readCdclFile) where

import           CDCL.Algorithm (cdcl)
import           CDCL.Types (CDCLResult (..), ClauseList)
import           Control.Monad
import           System.IO
import           System.TimeIt

readCdclFile :: String -> Bool -> Bool -> IO ()
readCdclFile path check check2 = do
    handle <- openFile path ReadMode
    f <- loopCheck handle []
    case f of
        Nothing -> putStrLn "Error. The given file doesn't contain a legitimate Content."
        Just s -> if check2 then timeIt $ print (cdcl s False True) else if check then timeIt $ print (cdcl s True False) else timeIt $ print (cdcl s False False)
    hClose handle

checkComment :: Char -> Bool
checkComment c = c == 'c'

checkCNFStart :: Char -> Bool
checkCNFStart c = c == 'p'

loopCheck :: Handle -> [[Integer]] -> IO (Maybe [[Integer]])
loopCheck handle clist = do
    end <- hIsEOF handle
    if end then
        pure Nothing
    else do
        f <- hGetChar handle
        if checkCNFStart f then
            do
                m <- hGetLine handle
                loopCheck' handle clist
        else do
            m <- hGetLine handle
            loopCheck handle clist

loopCheck' :: Handle -> [[Integer]] -> IO (Maybe [[Integer]])
loopCheck' handle clist = do
    end <- hIsEOF handle
    if end then
        pure (Just  (reverse clist))
    else do
        firstChar <- hGetChar handle
        if firstChar == '%' then
            pure (Just ( reverse clist))
        else if checkComment firstChar || firstChar == '\n' then
            do
                remove <- hGetLine handle
                loopCheck' handle clist
        else do
            content <- hGetLine handle
            let word = words (firstChar : content)
            let list = createIntegerList word []
            loopCheck' handle (list : clist)

createIntegerList :: [String] -> [Integer] -> [Integer]
createIntegerList (xString : ysString) intList
    | m == 0 = intList
    | otherwise = createIntegerList ysString (m : intList)
    where m = read xString :: Integer

createIntegerList [] ys = ys
