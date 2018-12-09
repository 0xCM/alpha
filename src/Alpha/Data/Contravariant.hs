module Alpha.Data.Contravariant
(
    Contravariant,
    contramap, (>$<), (>$$<),
    (>$),
    Predicate, predicate,
    Comparison, comparison,
    phantom
)

where
import Alpha.Base
import Data.Functor.Contravariant


predicate::(a -> Bool) -> Predicate a
predicate f = Predicate { getPredicate = f }

comparison::(a -> a -> Ordering) -> Comparison a
comparison f = Comparison {getComparison = f}

