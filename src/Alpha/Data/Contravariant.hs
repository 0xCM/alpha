-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
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

