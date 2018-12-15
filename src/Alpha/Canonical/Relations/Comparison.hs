module Alpha.Canonical.Relations.Comparison
(    
    (<=), (<), (>=), (>),

    Extremum(..),
    Infimum(..), Infimal(..),
    Supremum(..), Supremal(..),
    Minimal(..), Maximal(..),
    min, max

) where
import Alpha.Base
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations.Predicates
import Alpha.Canonical.Element
import qualified Numeric.Interval as Interval
import qualified Prelude as P

type family Infimum a
type family Supremum a    
type family Extremum a

type instance Extremum (Interval a) = a

-- | Characterizes a type for which a minimal element can be identified
-- i.e., a is minimal in A if a <= x for all x in A
class Minimal a where
    -- A minimal element 
    minimum::a -> Element a

-- | Characterizes a type for which a minimal element can be identified
-- i.e., a is maximal in A if a >= x for all x in A
class Maximal a where
    maximum::a -> Element a
    
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



(<=)::(Ord a) => BinaryPredicate a
(<=) a b= a P.<= b
infix 4 <=
    
(<)::(Ord a) => BinaryPredicate a
(<) a b = a P.< b
infix 4 <    

(>)::(Ord a) => BinaryPredicate a
(>) a b = a P.> b
infix 4 >

(>=)::(Ord a) => BinaryPredicate a
(>=) a b = a P.>= b

between::(Ord a) => TernaryPredicate a
between x a b = x >= a || x <= b
infix 4 >=    
    
-- Computes the minimum of two values    
min::(Ord a) => a -> a -> a
min x y = ifelse (x <= y) x y

-- Computes the maximum of two values
max::(Ord a) => a -> a -> a
max x y = ifelse (x >= y) x y
    
instance (Ord a, Semigroup a) => Minimal [a] where
    minimum (x:xs) = Min <$> (x :| xs) |> sconcat |> getMin

instance (Ord a, Semigroup a) => Maximal [a] where
    maximum (x:xs) = Max <$> (x :| xs) |> sconcat |> getMax
        
instance Infimal (Interval a) where
    infimum = Interval.inf

instance Supremal (Interval a) where
    supremum = Interval.sup
    
        