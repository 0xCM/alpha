{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Algebra.Unital
(
    Unital(..)    

) where
import Alpha.Base
import Alpha.Canonical.Operators
import Alpha.Native

class Unital a where
    -- | Specifies the unique element 1 such that 1*a = a*1 = a forall a
    one::a

-- Unital
-------------------------------------------------------------------------------
instance Unital Natural where 
    one = 1
    {-# INLINE one #-}

instance Unital Integer where 
    one = 1
    {-# INLINE one #-}

instance Unital Int where 
    one = 1
    {-# INLINE one #-}

instance Unital Int8 where 
    one = 1
    {-# INLINE one #-}

instance Unital Int16 where 
    one = 1
    {-# INLINE one #-}

instance Unital Int32 where 
    one = 1
    {-# INLINE one #-}

instance Unital Int64 where 
    one = 1
    {-# INLINE one #-}

instance Unital Word where 
    one = 1
    {-# INLINE one #-}

instance Unital Word8 where 
    one = 1
    {-# INLINE one #-}

instance Unital Word16 where 
    one = 1
    {-# INLINE one #-}

instance Unital Word32 where 
    one = 1
    {-# INLINE one #-}

instance Unital Word64 where 
    one = 1
    {-# INLINE one #-}

instance (Integral a) => Unital (Ratio a) where 
    one = 1
    {-# INLINE one #-}

instance Unital Float where 
    one = 1
    {-# INLINE one #-}

instance Unital Double where 
    one = 1
    {-# INLINE one #-}

instance Unital CFloat where 
    one = 1
    {-# INLINE one #-}

instance Unital CDouble where 
    one = 1
    {-# INLINE one #-}

instance (Ord a, Unital a) =>  Unital (ItemSet a) where
    one = [one]
    
---
type Unital2 a1 a2 = (Unital a1, Unital a2)
type Unital3 a1 a2 a3 = (Unital2 a1 a2, Unital a3)
type Unital4 a1 a2 a3 a4 = (Unital3 a1 a2 a3, Unital a4)
type Unital5 a1 a2 a3 a4 a5 = (Unital4 a1 a2 a3 a4, Unital a5)

instance Unital2 a1 a2 => Unital (a1,a2) where
    one = (one,one)

instance Unital3 a1 a2 a3 => Unital (a1,a2,a3) where
    one = (one,one,one)

instance Unital4 a1 a2 a3 a4 => Unital (a1,a2,a3,a4) where
    one = (one,one,one,one)

instance Unital5 a1 a2 a3 a4 a5 => Unital (a1,a2,a3,a4,a5) where
    one = (one,one,one,one,one)
            