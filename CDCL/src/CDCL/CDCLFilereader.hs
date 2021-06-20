module CDCL.CDCLFilereader (readCdclFile) where

import           CDCL.Algorithm (cdcl)
import           CDCL.Types (CDCLResult (..), ClauseList)
import           Control.Monad
import           System.IO

readCdclFile :: String -> String -> IO ()
readCdclFile path check = do
    handle <- openFile path ReadMode
    f <- loopCheck handle [] check
    case f of
        Nothing -> putStrLn "Error. The given file doesn't contain a legitimate Content."
        Just s -> print s
    hClose handle

checkComment :: Char -> Bool
checkComment c = c == 'c'

checkCNFStart :: Char -> Bool
checkCNFStart c = c == 'p'

loopCheck :: Handle -> [[Integer]] -> String -> IO (Maybe CDCLResult)
loopCheck handle clist stats = do
    end <- hIsEOF handle
    if end then
        pure Nothing
    else do
        f <- hGetChar handle
        if checkCNFStart f then
            do
                m <- hGetLine handle
                loopCheck' handle clist stats
        else do
            m <- hGetLine handle
            loopCheck handle clist stats

loopCheck' :: Handle -> [[Integer]] -> String -> IO (Maybe CDCLResult)
loopCheck' handle clist stats = do
    end <- hIsEOF handle
    if end then
        if stats == "yes" then pure (Just (cdcl clist True)) else pure (Just (cdcl clist False)) 
    else do
        firstChar <- hGetChar handle
        if firstChar == '%' then
            if stats == "yes" then pure (Just (cdcl clist True)) else pure (Just (cdcl clist False))
        else if checkComment firstChar || firstChar == '\n' then
            do
                remove <- hGetLine handle
                loopCheck' handle clist stats
        else do
            content <- hGetLine handle
            let word = words (firstChar : content)
            let list = createIntegerList word []
            loopCheck' handle (list : clist) stats

createIntegerList :: [String] -> [Integer] -> [Integer]
createIntegerList (xString : ysString) intList
    | m == 0 = intList
    | otherwise = createIntegerList ysString (m : intList)
    where m = read xString :: Integer

createIntegerList [] ys = ys
