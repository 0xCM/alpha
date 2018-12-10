module Alpha.Canonical.Algebra.Extremal
(
    
    Extremum(..),
    Infimum(..), Infimal(..),
    Supremum(..), Supremal(..),
    minimal, maximal,
    

) where
import Alpha.Base
import Alpha.Canonical.Relations
import Alpha.Canonical.Operators
import qualified Numeric.Interval as Interval

type family Infimum a
type family Supremum a    
type family Extremum a

type instance Extremum (Interval a) = a



-- / Characterizes types for which a greatest lower bound can
-- be identified, with bounded intervals being the canonical
-- example
-- See https://en.wikipedia.org/wiki/Infimum_and_supremum    
class Infimal a where
    -- / The greatest lower bound
    infimum::a -> Extremum a
    

-- / Characterizes types for which a least upper bound can
-- be identified, with bounded intervals being the canonical
-- example
-- See https://en.wikipedia.org/wiki/Infimum_and_supremum    
class Supremal a where
    -- / The least upper bound
    supremum::a -> Extremum a

minimal::(Ord a, Semigroup a) => [a] -> Min a
minimal (x:xs) = Min <$> (x :| xs) |> sconcat

maximal::(Ord a, Semigroup a) => [a] -> Max a
maximal (x:xs) = Max <$> (x :| xs) |> sconcat
    
instance Infimal (Interval a) where
    infimum = Interval.inf

instance Supremal (Interval a) where
    supremum = Interval.sup
    