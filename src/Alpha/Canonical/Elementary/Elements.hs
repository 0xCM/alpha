-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}

module Alpha.Canonical.Elementary.Elements
(
    Element(..),
    Structure(..),    
    Set(..),
    Enumerable(..),
    Subset(..),
    Counted(..),
    Vectored(..),
    Substructure(..),
    Discrete(..),
    
    
)
where
import Alpha.Base

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Sequence as Sequence

type family Element a
type instance Element [a] = a


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
class Set a => Enumerable a where
    enumerate::[a]

-- | Classifies a type that defines rules for membership
-- inclusion. This is in contradistinction to a set which
-- merely contains elements
class (Eq (Individual a) ) => Structure a where
    type Individual a        

class (Structure a, Subset b a) => Substructure a b where
    substructure::a -> b

-- | Characterizes a type inhabited by a finite set of
-- values and for which a count is determined
class Counted a where
    -- | Counts the number of items within the purview of the subject
    count::(Integral n) => a -> n

class Structure a => Discrete a where
    -- | Extracts the elements from a structure
    members::a -> [Individual a]
        
    
class Vectored a where
    vector::a -> Vector (Element a)    
    
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

instance Structure Integer where
    type Individual Integer = Integer
instance Structure Int where
    type Individual Int = Int
instance Structure Int8 where
    type Individual Int8 = Int8
instance Structure Int16 where
    type Individual Int16 = Int16
instance Structure Int32 where
    type Individual Int32 = Int32
instance Structure Int64 where
    type Individual Int64 = Int64   
instance (Integral a) => Structure (Ratio a) where
    type Individual (Ratio a) = Ratio a
instance Structure Float where
    type Individual Float = Float
instance Structure Double where
    type Individual Double = Double
instance Structure CFloat where
    type Individual CFloat = CFloat
instance Structure CDouble where
    type Individual CDouble = CDouble
instance Structure Natural where
    type Individual Natural = Natural
instance Structure Word where
    type Individual Word = Word
instance Structure Word8 where
    type Individual Word8 = Word8
instance Structure Word16 where
    type Individual Word16 = Word16
instance Structure Word32 where
    type Individual Word32 = Word32
instance Structure Word64 where
    type Individual Word64 = Word64

instance Enumerable Int where
    enumerate =  [minBound .. maxBound]                
instance Enumerable Word where
    enumerate =  [minBound .. maxBound]
instance Enumerable Word8 where
    enumerate =  [minBound .. maxBound]
instance Enumerable Word16 where
    enumerate =  [minBound .. maxBound]
instance Enumerable Word32 where
    enumerate =  [minBound .. maxBound]
instance Enumerable Word64 where
    enumerate =  [minBound .. maxBound]
instance Enumerable Int8 where
    enumerate =  [minBound .. maxBound]
instance Enumerable Int16 where
    enumerate =  [minBound .. maxBound]
instance Enumerable Int32 where
    enumerate =  [minBound .. maxBound]
instance Enumerable Int64 where
    enumerate =  [minBound .. maxBound]                                    
instance Enumerable Natural where
    enumerate =  [0 .. ]
