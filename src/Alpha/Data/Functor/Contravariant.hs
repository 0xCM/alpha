module Alpha.Data.Functor.Contravariant 
(
    Contravariant,
    contramap, (>$<), (>$$<),
    (>$),
    Predicate, predicate,
    Comparison, comparison,
    OpMap, opmap,
    phantom
)

where
import Alpha.Base

import Data.Functor.Contravariant

type OpMap = Op

predicate::(a -> Bool) -> Predicate a
predicate f = Predicate { getPredicate = f }

comparison::(a -> a -> Ordering) -> Comparison a
comparison f = Comparison {getComparison = f}

opmap::(b -> a) -> OpMap a b
opmap f = Op {getOp = f}
