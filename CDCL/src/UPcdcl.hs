module UPcdcl (unitPropagation) where

import           Types (ClauseList, TupelList)
import           Unitpropagation (getUnitClause, setVariable, unitResolution,
                     unitSubsumption)


unitPropagation :: ClauseList -> TupelList -> (ClauseList , TupelList)
unitPropagation clist tlist
    | null clist || null unitClause = (clist, tlist)
    | otherwise = unitPropagation resolutionC (tlist ++ [calcTuple])
    where unitClause = getUnitClause clist
          calcTuple = setVariable unitClause
          fstTuple = fst calcTuple
          subsumptionC = unitSubsumption clist calcTuple
          resolutionC = unitResolution subsumptionC calcTuple
