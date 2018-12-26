-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Elementary.Sets
(
    Set(..),
    Individual(..),
    SetBuilder(..),
    Subset(..),
    DiscreteSet(..),
    FiniteSet(..),
    DiscreteSubset(..),
    NonEmptySet(..),
    FiniteNonEmptySet(..),
    subset'
)
where
import Alpha.Canonical.Common
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Sequence as Sequence

type family Individual a
type instance Individual [a] = a
type instance Individual (Set a) = a
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


-- | Classifies a type that can be interpreted as a set
-- where members can be distinquished from one another
-- via 'Eq'. Similar to a 'Setoid' where the equivalance
-- operation is equality.
--class Eq a => Set a where
data Set a 
    -- | Constructs a set representative
    = SetRep
    -- | Constructs a set with explicit content
    | DataSet [a]
    -- | Constructs the empty set for the type parameter
    | EmptySet
    deriving(Generic, Data, Typeable)

-- | Classifies a type whose values can be interpreted as a 
-- subset of another type's values
class Subset a b where
    subset::Set b -> Set a

-- | Characterizes a discrete set    
class DiscreteSet a where
    individuals::Set a -> [Individual a]    

-- | Classifies a type for which a discrete set of values can be interpreted as a 
-- subset of another type's values
class (Subset a b, DiscreteSet a) => DiscreteSubset a b where
    subelements::Set b -> [Individual a]
    subelements b = individuals  (subset @a b)
    

-- | Characterizes a type inhabited by a finite set of
-- values and for which a count is determined
class FiniteSet a where

    -- | Counts the number of items within the purview of the subject
    count::(Integral n) => Set a -> n
    
-- | Characterizes a nonempty set    
class NonEmptySet a where

    -- | Retrives the leading (first) item in the set which
    -- is guaranteed to exist
    leading::a -> Individual a    

-- | Characterizes a nonempty finite collection    
class (NonEmptySet a, FiniteSet a) => FiniteNonEmptySet a where


class SetBuilder s a where
    -- | Extracts the elements from a structure
    set::s -> [a]        

class SetFamily f i a where
    sets::f -> [(i,[a])]

instance (Formattable a,Typeable a) => Formattable (Set a) where
    format s = Text.pack (show $ typeOf s)
    
instance (Formattable a,Typeable a) => Show (Set a) where
    show = Text.unpack . format 
    
subset'::forall a b. Subset a b => Set a
subset' = subset @a (SetRep @b)

emptyset::Set a
emptyset = EmptySet

dataset::[a] -> Set a
dataset items = if (List.null items) then EmptySet else DataSet items

setrep::Set a
setrep = SetRep


instance DiscreteSet Int8 where
    individuals _ = [minBound .. maxBound]
instance DiscreteSet Int16 where
    individuals _ = [minBound .. maxBound]
instance DiscreteSet Int32 where
    individuals _ = [minBound .. maxBound]
instance DiscreteSet Int64 where
    individuals _ = [minBound .. maxBound]
instance DiscreteSet Word8 where
    individuals _ = [minBound .. maxBound]
instance DiscreteSet Word16 where
    individuals _ = [minBound .. maxBound]
instance DiscreteSet Word32 where
    individuals _ = [minBound .. maxBound]
instance DiscreteSet Word64 where
    individuals _ = [minBound .. maxBound]
instance DiscreteSet Natural where
    individuals _ = [0..]

instance FiniteSet Int8 where
    count  =  fromIntegral . List.length . individuals
instance FiniteSet Int16 where
    count  =  fromIntegral . List.length . individuals
instance FiniteSet Int32 where
    count  =  fromIntegral . List.length . individuals
instance FiniteSet Int64 where
    count  =  fromIntegral . List.length . individuals
instance FiniteSet Word8 where
    count  =  fromIntegral . List.length . individuals
instance FiniteSet Word16 where
    count  =  fromIntegral . List.length . individuals 
instance FiniteSet Word32 where
    count  =  fromIntegral . List.length . individuals 
instance FiniteSet Word64 where
    count  =  fromIntegral . List.length . individuals 
    
instance Subset Int8 Int16 where
    subset _ = SetRep @Int8
instance Subset Int16 Int32 where
    subset _ = SetRep @Int16
instance Subset Int32 Int64 where
    subset _ = SetRep @Int32
instance Subset Int64 Integer where
    subset _ = SetRep @Int64
instance Subset Word8 Word16 where
    subset _ = SetRep @Word8
instance Subset Word16 Word32 where
    subset _ = SetRep @Word16
instance Subset Word32 Word64 where
    subset _ = SetRep @Word32
instance Subset Word64 Natural where
    subset _ = SetRep @Word64
instance Subset Natural Integer where
    subset _ = SetRep @Natural
                    
