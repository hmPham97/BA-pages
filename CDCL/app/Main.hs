module Main where

import System.IO  
import Control.Monad
import CDCL.CDCLFilereader (readCdclFile)

main :: IO()
main = do readCdclFile "test.cnf"
