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

import           CDCL.Types (ClauseList, TupelList)
import           CDCL.Unitpropagation (getUnitClause, setVariable,
                     unitResolution, unitSubsumption)

-- | Mainlogic of the unitpropagation. It calls every important function
--   necessary for the CDCL Function.
unitPropagation :: ClauseList -> TupelList -> (ClauseList , TupelList)
unitPropagation clist tlist
    | null clist || null unitClause = (clist, tlist)
    | otherwise = unitPropagation resolutionC (tlist ++ [calcTuple])
    where unitClause = getUnitClause clist
          calcTuple = setVariable unitClause
          fstTuple = fst calcTuple
          subsumptionC = unitSubsumption clist calcTuple
          resolutionC = unitResolution subsumptionC calcTuple
