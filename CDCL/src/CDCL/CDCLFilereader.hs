module CDCL.CDCLFilereader (readCdclFile) where

import           CDCL.Algorithm (cdcl)
import           CDCL.Types (CDCLResult (..), ClauseList)
import           Control.Monad
import           System.IO
import           System.TimeIt

readCdclFile :: String -> Bool -> IO ()
readCdclFile path check = do
    handle <- openFile path ReadMode
    f <- loopCheck handle []
    case f of
        Nothing -> putStrLn "Error. The given file doesn't contain a legitimate Content."
        Just s -> if check then timeIt $ print (cdcl s check) else print (cdcl s check)
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
        --if stats == "yes" then pure (Just (cdcl clist True)) else pure (Just (cdcl clist False)) 
    else do
        firstChar <- hGetChar handle
        if firstChar == '%' then
            pure (Just ( reverse clist))
            --if stats == "yes" then pure (Just (cdcl clist True)) else pure (Just (cdcl clist False))
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
