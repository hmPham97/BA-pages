module Types (Clause, ClauseList, Tupel, TupelList, Level) where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

type Clause = [Int]
type ClauseList = [Clause]
type Tupel = (Int, Int)
type TupelList = [Tupel]
type Level = Int
type MappedTupleList = IntMap.IntMap TupelList
