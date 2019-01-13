-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
module Alpha.Canonical.Algebra.Numeric
(
    module X,
    Numeric(..),
    Number(..), 
    number,
) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Powers as X

type NumericContext a = (Ord a, Subtractive a, Distributive a, Multiplicative a, Additive a, Divisive a, Num a, Real a, Power a)    

class NumericContext a => Numeric a where
    num::a -> a
    num = id
    {-# INLINE num #-}

newtype Number a = Number a
    deriving (Eq,Ord)
        
number::(Numeric a) => a -> Number a
number = Number

-------------------------------------------------------------------------------
-- * Number class membership
-------------------------------------------------------------------------------
deriving instance Num a  => Num (Number a)
deriving instance Subtractive a  => Subtractive (Number a)
deriving instance Additive a  => Additive (Number a)
deriving instance Nullary a  => Nullary (Number a)
deriving instance Multiplicative a  => Multiplicative (Number a)
deriving instance Unital a  => Unital (Number a)
deriving instance Divisive a  => Divisive (Number a)
deriving instance Real a  => Real (Number a)
deriving instance Power a  => Power (Number a)
deriving instance LeftDistributive a  => LeftDistributive (Number a)
deriving instance RightDistributive a  => RightDistributive (Number a)
deriving instance Numeric a => Numeric (Number a)
deriving instance LT a => LT (Number a)    
deriving instance GT a => GT (Number a)    
deriving instance LTEQ a => LTEQ (Number a)    
deriving instance GTEQ a => GTEQ (Number a)    
deriving instance Comparable a => Comparable (Number a)    

-------------------------------------------------------------------------------
-- * Numeric instances
-------------------------------------------------------------------------------
instance Numeric Natural
instance Numeric Integer
instance Numeric Int
instance Numeric Int8
instance Numeric Int16
instance Numeric Int32
instance Numeric Int64
instance Numeric Word
instance Numeric Word8
instance Numeric Word16
instance Numeric Word32
instance Numeric Word64
instance (Integral a, Ord a) => Numeric (Ratio a)
instance Numeric Float
instance Numeric Double
instance Numeric CFloat
instance Numeric CDouble







    
    