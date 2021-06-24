-- | Dieses Modul wird wahrscheinlich gelöscht
-- | Dieses Modul wird wahrscheinlich gelöscht
-- | Dieses Modul wird wahrscheinlich gelöscht

module CDCL.RUP where 

import CDCL.Types
import CDCL.MapLogic

startRUP :: ClauseList -> TupleClauseList -> MappedTupleList -> [Clause]
startRUP = rup

rup :: ClauseList -> TupleClauseList -> MappedTupleList  -> [Clause]
rup _ _ _ = []

setToFalse :: ClauseList -> MappedTupleList -> (ClauseList, MappedTupleList) 
setToFalse (xs : ys) mtl = (ys , setToFalse' (getClauseFromReducedClauseAndOGClause xs) mtl)


setToFalse' :: Clause -> MappedTupleList -> MappedTupleList 
setToFalse' (xs : ys) mtl = setToFalse' ys updated
    where curX = getVariableValue xs
          updated = if curX < 0 then pushToMappedTupleList mtl (Level 0) (xs, BTrue) Decision else pushToMappedTupleList mtl (Level 0) (xs, BFalse) Decision

setToFalse' [] mtl = mtl

