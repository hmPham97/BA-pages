module CDCL.CDCLFilereader (readCdclFile) where

import           CDCL.Algorithm (cdcl)
import           CDCL.Types (CDCLResult (..))
import           Control.Monad
import           System.IO

readCdclFile :: String -> IO ()
readCdclFile path = do
    handle <- openFile path ReadMode
    f <- loopCheck handle
    case f of
        Nothing -> putStrLn "error"
        Just s -> print s
    hClose handle

checkComment :: Char -> Bool
checkComment c = c == 'c'

checkCNFStart :: Char -> Bool
checkCNFStart c = c == 'p'

loopCheck :: Handle -> IO (Maybe CDCLResult)
loopCheck handle = do
    f <- hGetChar handle
    if checkCNFStart f then
        do
            m <- hGetLine handle
            content <- hGetContents handle
            let res = createClauseList content
            pure (Just res)
    else do
        m <- hGetLine handle
        loopCheck handle

createClauseList :: String -> CDCLResult
createClauseList s
    | not (null cl) = cdcl cl
    | otherwise = error "error in file"
    where m = words s
          cl = filter (not . null) (createClauseList' m [[]])

createClauseList' :: [String] -> [[Integer]] -> [[Integer]]
createClauseList' (xString : ysString) intList
    | m == 0 = createClauseList' ysString (intList ++ [[]])
    | otherwise = createClauseList' ysString (f ++ [lastList])
    where m = read xString :: Integer
          f = init intList
          lastList = last intList ++ [m]

createClauseList' [] ys = ys
