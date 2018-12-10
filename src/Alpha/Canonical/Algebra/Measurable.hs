module Alpha.Canonical.Algebra.Measurable
(
    Measurable(..),
    Dimensional(..),    
    Length(..),
    Vapid(..),
    Counted(..)

) where
import Alpha.Base
import Alpha.Canonical.Operators
import qualified Data.List as List
import qualified Data.Text as Text

-- | Characterizes measurable things, in the spirit, but not formally, of Lebesque
class Measurable (n::Nat) a where
    measure::forall b. (Num b) => a -> b

-- Characterizes a type for which a notion of dimensionality 
-- can be defined, e.g., an array, matrix or more generally a tensor
class Dimensional a where
    type Dimension a
    dimension::a -> Dimension a

class Length a where    
    length::forall b. (Num b) => a -> b
    
-- | Defines membership predicated on the ability to be counted by an existential machine
class Counted a where
    -- | Counts the number of items within the purview of the subject
    count::(Integral n) => a -> n

-- Characterizes types that are inhabited by 'degenerate'  or "empty" values
-- Examples include empty lists, mathematical intervals 
-- that represent a single value, etc. What precicely constitutes a 
-- a degenerate value for a given type is implementation-defined
-- See https://en.wikipedia.org/wiki/Degeneracy_(mathematics)
class Vapid a where
    empty::a -> Bool

instance Length a => Measurable 1 a where
    measure = length
    
instance Length [a] where
    length x = List.length x |> fromIntegral
    
instance Vapid [a] where
    empty a = List.length a == 0
    
instance Length Text where
    length t =   Text.length t |> fromIntegral
    
instance Length Char where
    length c = 1
    