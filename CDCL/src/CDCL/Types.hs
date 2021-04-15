---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Types
-- Description :   Contains type declaration for CDCL Package
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :   
-- Maintainer  :   
-- Stability   :   
-- Portability :   
-- 
---------------------------------------------------------------------
module CDCL.Types (Clause, ClauseList, Tupel, TupelList, Level, Activity, ActivityMap, VariableActivity) where

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
