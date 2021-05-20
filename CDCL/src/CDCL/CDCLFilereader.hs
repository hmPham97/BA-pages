module CDCL.CDCLFilereader (readCdclFile) where

import           CDCL.Algorithm (cdcl)
import           CDCL.Types (CDCLResult (..), ClauseList)
import           Control.Monad
import           System.IO

readCdclFile :: String -> IO ()
readCdclFile path = do
    putStrLn ("Reading file " ++ path)
    putStrLn ""
    handle <- openFile path ReadMode
    f <- loopCheck handle []
    case f of
        Nothing -> putStrLn "Error. The given file doesn't contain a legitimate Content."
        Just s -> print s
    hClose handle

checkComment :: Char -> Bool
checkComment c = c == 'c'

checkCNFStart :: Char -> Bool
checkCNFStart c = c == 'p'

loopCheck :: Handle -> [[Integer]] -> IO (Maybe CDCLResult)
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

loopCheck' :: Handle -> [[Integer]] -> IO (Maybe CDCLResult)
loopCheck' handle clist = do
    end <- hIsEOF handle
    if end then
        pure (Just (cdcl clist))
    else do
        firstChar <- hGetChar handle
        if checkComment firstChar then
            do
                remove <- hGetLine handle
                loopCheck' handle clist
        else do
            content <- hGetLine handle
            let word = words (firstChar : content)
            let list = createIntegerList word []
            loopCheck' handle (clist ++ [list])

createIntegerList :: [String] -> [Integer] -> [Integer]
createIntegerList (xString : ysString) intList
    | m == 0 = intList
    | otherwise = createIntegerList ysString lastList
    where m = read xString :: Integer
          lastList = intList ++ [m]

createIntegerList [] ys = ys
