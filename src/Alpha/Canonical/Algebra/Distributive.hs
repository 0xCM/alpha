module Alpha.Canonical.Algebra.Distributive
(
    LeftDistributive(..),
    RightDistributive(..),
    Distributive(..)
) where
import Alpha.Base
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Multiplicative

class (Multiplicative a, Additive a) => LeftDistributive a where
    distL::a -> (a,a) -> a
    distL x (y1,y2) = x*y1 + x*y2
    {-# INLINE distL #-}

class (Multiplicative a, Additive a) => RightDistributive a where
    distR::(a,a) -> a -> a
    distR (x1,x2) y = x1*y + x2*y
    {-# INLINE distR #-}

class (LeftDistributive a, RightDistributive a) => Distributive a where
    dist::a -> (a,a) -> a
    dist = distL
    {-# INLINE dist #-}

instance LeftDistributive Natural
instance LeftDistributive Integer
instance LeftDistributive Int where 
instance LeftDistributive Int8 where 
instance LeftDistributive Int16 where 
instance LeftDistributive Int32 where 
instance LeftDistributive Int64 where 
instance LeftDistributive Word where 
instance LeftDistributive Word8 where 
instance LeftDistributive Word16 where 
instance LeftDistributive Word32 where 
instance LeftDistributive Word64 where 
instance (Integral a) => LeftDistributive (Ratio a) where 
instance LeftDistributive Float where 
instance LeftDistributive Double where 
instance LeftDistributive CFloat where 
instance LeftDistributive CDouble where 

instance RightDistributive Natural
instance RightDistributive Integer
instance RightDistributive Int where 
instance RightDistributive Int8 where 
instance RightDistributive Int16 where 
instance RightDistributive Int32 where 
instance RightDistributive Int64 where 
instance RightDistributive Word where 
instance RightDistributive Word8 where 
instance RightDistributive Word16 where 
instance RightDistributive Word32 where 
instance RightDistributive Word64 where 
instance (Integral a) => RightDistributive (Ratio a) where 
instance RightDistributive Float where 
instance RightDistributive Double where 
instance RightDistributive CFloat where 
instance RightDistributive CDouble where 

instance Distributive Natural
instance Distributive Integer
instance Distributive Int where 
instance Distributive Int8 where 
instance Distributive Int16 where 
instance Distributive Int32 where 
instance Distributive Int64 where 
instance Distributive Word where 
instance Distributive Word8 where 
instance Distributive Word16 where 
instance Distributive Word32 where 
instance Distributive Word64 where 
instance (Integral a) => Distributive (Ratio a) where 
instance Distributive Float where 
instance Distributive Double where 
instance Distributive CFloat where 
instance Distributive CDouble where 
    
