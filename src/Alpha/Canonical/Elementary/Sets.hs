-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Elementary.Sets
(
    Set(..),
    IntrinsicSet(..),
    SetBuilder(..),
    Subset(..),
)
where
import Alpha.Base

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Sequence as Sequence

-- | Classifies a type that can be interpreted as a set
-- where members can be distinquished from one another
-- via 'Eq'. Similar to a 'Setoid' where the equivalance
-- operation is equality.
class Eq a => Set a where

-- | Classifies a type whose values can be interpreted as a 
-- subset of another type's values
class (Set a, Set b) => Subset a b where
    subset::a -> b

-- | Classifies a type whose values can be enumerated
-- in the abscence of external state
class Set a => IntrinsicSet a where
    values::[a]
    default values::(Ord a, Enum a, Bounded a) => [a]
    values = [minBound .. maxBound]

class SetBuilder s a where
    -- | Extracts the elements from a structure
    set::s -> [a]        

class Set i => SetFamily f i a where
    sets::f -> [(i,[a])]

instance (Eq a) => Set [a]

instance Set Integer
instance Set Int
instance Set Int8
instance Set Int16
instance Set Int32
instance Set Int64
instance (Integral a) => Set (Ratio a)
instance Set Float
instance Set Double
instance Set CFloat
instance Set CDouble
instance Set Natural
instance Set Word
instance Set Word8
instance Set Word16
instance Set Word32
instance Set Word64


instance IntrinsicSet Int
instance IntrinsicSet Word
instance IntrinsicSet Word8
instance IntrinsicSet Word16
instance IntrinsicSet Word32
instance IntrinsicSet Word64
instance IntrinsicSet Int8
instance IntrinsicSet Int16
instance IntrinsicSet Int32
instance IntrinsicSet Int64
instance IntrinsicSet Natural where
    values =  [0 .. ]
    