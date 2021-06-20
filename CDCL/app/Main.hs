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
        putStrLn "Do you want more statistics? Enter \'yes\' or \'no\'"
        check <- getLine 
        timeIt $ readCdclFile h check
        --readCdclFile "test.cnf"
