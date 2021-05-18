module Main where

import           CDCL.CDCLFilereader (readCdclFile)
import           Control.Monad
import           Data.Char
import           System.IO

main :: IO()
main = do
        h <- getLine
        readCdclFile h
        -- do readCdclFile "test.cnf" 
