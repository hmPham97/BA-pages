module Main where

import           CDCL.CDCLFilereader (readCdclFile)
import           Control.Monad
import           Data.Char
import           System.IO
import           System.TimeIt

main :: IO()
main = do
        putStrLn "Enter the path to the file you want to read\n"
        h <- getLine
        putStrLn ("Reading file " ++ h)
        putStrLn ""
        putStrLn "Do you want more statistics? Enter \'yes\',\'no\' or \'help\'\n"
        check <- getLine 
        checkInput h check
        --readCdclFile "test.cnf"

checkInput :: String -> String -> IO()
checkInput handle input = 
        if input == "help" then do
                putStrLn "Statistic which are aditionally shown are:\nList of Decisions\nAmount of learned Clauses and the clause itself\nThe CPU time\n"
                putStrLn "Enter \'yes\',\'no\' or \'help\'\n"
                check <- getLine
                checkInput handle check
        else readCdclFile handle input