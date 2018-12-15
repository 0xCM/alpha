module Alpha.Canonical.Algebra.Nullary
(
    Nullary(..)
    
) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Operators
import Alpha.Canonical.Algebra.Additive

import qualified Data.Set as Set
import qualified Data.MultiSet as Bag
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Numeric.Interval as Interval


class Nullary a where
    -- | Specifies the unique element 0 such that 0 + a = a + 0 = a forall a
    zero::a

    -- Tests whether a value is equal to the canonical zero
    isZero::(Eq a) => a -> Bool
    isZero a = a == zero

instance Nullary Natural where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Integer where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Int where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Int8 where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Int16 where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Int32 where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Int64 where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Word where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Word8 where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Word16 where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Word32 where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Word64 where 
    zero = 0
    {-# INLINE zero #-}

instance (Integral a) => Nullary (Ratio a) where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Float where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Double where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary CFloat where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary CDouble where 
    zero = 0
    {-# INLINE zero #-}

-- Nullary tuples
type Nullary2 a1 a2 = (Nullary a1, Nullary a2)
type Nullary3 a1 a2 a3 = (Nullary2 a1 a2, Nullary a3)
type Nullary4 a1 a2 a3 a4 = (Nullary3 a1 a2 a3, Nullary a4)
type Nullary5 a1 a2 a3 a4 a5 = (Nullary4 a1 a2 a3 a4, Nullary a5)
    
instance Nullary2 a1 a2 => Nullary (a1,a2) where
    zero = (zero,zero)

instance Nullary3 a1 a2 a3 => Nullary (a1,a2,a3) where
    zero = (zero,zero,zero)

instance Nullary4 a1 a2 a3 a4 => Nullary (a1,a2,a3,a4) where
    zero = (zero,zero,zero,zero)

instance Nullary5 a1 a2 a3 a4 a5 => Nullary (a1,a2,a3,a4,a5) where
    zero = (zero,zero,zero,zero,zero)
