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

import           CDCL.Types (ClauseList, Level, MappedTupleList, TupelList, TriTuple, Reason(..))
import qualified CDCL.Types as TypeC

import           CDCL.Unitpropagation (getUnitClause, pushToMappedTupleList,
                     setVariable, unitResolution, unitSubsumption)
import qualified CDCL.Unitpropagation as Unitpropagation

-- | Mainlogic of the unitpropagation. It calls every important function
--   necessary for the CDCL Function.
unitPropagation :: ClauseList -> TupelList -> Level -> MappedTupleList -> TriTuple
unitPropagation clist tlist lvl mapped
    | null clist || null unitClause = (clist, tlist, mapped)
    | otherwise = unitPropagation resolutionC (tlist ++ [(calcTuple, Reason unitClause)]) lvl updatedMap
    where unitClause = getUnitClause clist
          calcTuple = setVariable unitClause
          fstTuple = fst calcTuple
          updatedMap = pushToMappedTupleList mapped lvl calcTuple (Reason unitClause)
          subsumptionC = unitSubsumption clist (calcTuple, Reason unitClause)
          resolutionC = unitResolution subsumptionC (calcTuple, Reason unitClause)
