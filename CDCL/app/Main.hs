module Main where

import           CDCL.CDCLFilereader (readCdclFile)
import           Data.Semigroup ((<>))
import           Options.Applicative

main :: IO ()
main = input =<< execParser opts
  where
    opts = info (inputParse <**> helper)
      ( fullDesc
     <> progDesc "Print a CDCL Result for TARGETFILE"
     <> header "Starting CDCL-SAT-Solver via Commandline " )

input :: CDCLInput -> IO ()
input (CDCLInput target False) = readCdclFile target False
input (CDCLInput target True) = readCdclFile target True


data CDCLInput = CDCLInput
  { file :: String
  , opts :: Bool
  }

inputParse :: Parser CDCLInput
inputParse = CDCLInput
      <$> strOption
          ( long "input"
         <> short 'i'
         <> metavar "TARGETFILE"
         <> help "Target cnf-file which will be analysed from SAT-Solver" )
      <*> switch
          ( long "stats"
         <> short 's'
         <> help "Showing additional statistics")
