---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Algorithm
-- Description :   Contains main logic for unitpropagation.
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :
-- Maintainer  :
-- Stability   :
-- Portability :
--
---------------------------------------------------------------------
module CDCL.UPcdcl (unitPropagation) where

import           CDCL.Types (ClauseList, Level, MappedTupleList, TupelList, TriTuple)
import           CDCL.Unitpropagation (getUnitClause, pushToMappedTupleList,
                     setVariable, unitResolution, unitSubsumption)

-- | Mainlogic of the unitpropagation. It calls every important function
--   necessary for the CDCL Function.
unitPropagation :: ClauseList -> TupelList -> Level -> MappedTupleList -> TriTuple
unitPropagation clist tlist lvl mapped
    | null clist || null unitClause = (clist, tlist, mapped)
    | otherwise = unitPropagation resolutionC (tlist ++ [calcTuple]) lvl updatedMap
    where unitClause = getUnitClause clist
          calcTuple = setVariable unitClause
          fstTuple = fst calcTuple
          updatedMap = pushToMappedTupleList mapped lvl calcTuple
          subsumptionC = unitSubsumption clist calcTuple
          resolutionC = unitResolution subsumptionC calcTuple
