module Alpha.Canonical.Common.Individual
(
    Individual(..),
    Associated(..),
    Discrete(..),
    Finite(..),
    Componentized(..),

)
where
import Alpha.Canonical.Common.Root
import qualified Data.List as List

type family Individual a
type instance Individual [a] = a
type instance Individual Integer = Integer
type instance Individual Int = Int
type instance Individual Int8 = Int8
type instance Individual Int16 = Int16
type instance Individual Int32 = Int32
type instance Individual Int64 = Int64
type instance Individual (Ratio a) = Ratio a
type instance Individual Natural = Natural
type instance Individual Word = Word
type instance Individual Word8 = Word8
type instance Individual Word16 = Word16
type instance Individual Word32 = Word32
type instance Individual Word64 = Word64
    
-- | Characterizes a type that is comprised of individuals or
-- can be discretized as such
class Discrete a where
    individuals::a -> [Individual a]

-- | Characterizes a type inhabited by a finite set of
-- values and for which a count is determined
class (Discrete a) => Finite a where
    -- | Counts the number of items within the purview of the subject
    count::(Integral n) => a -> n
    count a = individuals a |> List.length |> fromIntegral
        
-- | Characterizes a type that defines an association among a 
-- collection of individuals
class Associated a where

    -- | Specifies the members that participate in the association
    associates::a -> [Individual a]    

-- / Characterizes a type that owns a collection of individuals
class Componentized a where

    -- | Extracts the components that are owned by the whole
    components::a -> [Individual a]    

