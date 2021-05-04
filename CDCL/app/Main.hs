module Main where

import CDCL.Algorithm (cdcl)

main :: IO()
main = print (cdcl [[11,1,2,51],[23,-2]])