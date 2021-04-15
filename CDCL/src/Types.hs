module Types (Clause, ClauseList, Tupel, TupelList, Level, Activity, ActivityMap, VariableActivity) where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

type Variable = Int
type Clause = [Variable]
type ClauseList = [Clause]

type Tupel = (Int, Int)
type TupelList = [Tupel]

type Level = Int
type MappedTupleList = IntMap.IntMap TupelList

type Activity = Int
type ActivityMap = IntMap.IntMap Activity
type VariableActivity = (Variable, Activity)
