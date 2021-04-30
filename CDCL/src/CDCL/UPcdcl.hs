---------------------------------------------------------------------
-- |
-- Module      :   CDCL.Algorithm
-- Description :   Contains main logic for unitpropagation.
-- Copyright   :   (c) Thanh Nam Pham, 2021
-- License     :   Apache-2.0
-- Maintainer  :
-- Stability   :
-- Portability :
--
---------------------------------------------------------------------
module CDCL.UPcdcl (unitPropagation) where

import           CDCL.Types (ClauseList, Level, MappedTupleList, TupelClauseList, TriTuple, Reason(..))
import qualified CDCL.Types as TypeC

import           CDCL.Unitpropagation (getUnitClause, pushToMappedTupleList,
                     setVariable, unitResolution, unitSubsumption)
import qualified CDCL.Unitpropagation as Unitpropagation

-- | Mainlogic of the unitpropagation. It calls every important function
--   necessary for the CDCL Function.
unitPropagation :: ClauseList -> TupelClauseList -> Level -> MappedTupleList -> TriTuple
unitPropagation clist tlist lvl mapped
    | null clist || null (fst unitClause) = (clist, tlist, mapped)
    | otherwise = unitPropagation resolutionC (tlist ++ [(calcTuple, ogClause)]) lvl updatedMap
    where unitClause = getUnitClause clist
          calcTuple = setVariable (fst unitClause)
          fstTuple = fst calcTuple
          ogClause = Reason (snd unitClause)
          updatedMap = pushToMappedTupleList mapped lvl calcTuple ogClause
          subsumptionC = unitSubsumption clist (calcTuple, ogClause)
          resolutionC = unitResolution subsumptionC (calcTuple, ogClause)
