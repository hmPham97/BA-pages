module Main where

import           CDCL.CDCLFilereader (readCdclFile)
import           Control.Monad
import           Data.Char
import           System.IO

main :: IO()
main = do
        putStrLn "Enter the path to the file you want to read\n"
        h <- getLine
        readCdclFile h
        --readCdclFile "test.cnf"
