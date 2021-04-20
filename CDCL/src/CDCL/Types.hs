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
module CDCL.Types where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data DPLLResult
    =
        DResolved
    |
        DNotResolved
    deriving(Eq, Ord, Show)

data CDCLResult
    =
        SAT TupelList
    |
        UNSAT
    deriving(Eq, Ord, Show)

type Variable = Integer
type Clause = [Variable]
type ClauseList = [Clause]

type Tupel = (Integer, Integer)
type TupelList = [Tupel]

type Level = Integer
type MappedTupleList = Map.Map Integer TupelList

type Activity = Integer
type ActivityMap = Map.Map Integer Activity
type VariableActivity = (Variable, Activity)
