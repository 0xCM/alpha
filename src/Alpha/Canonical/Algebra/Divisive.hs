-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Divisive
(
    Divisive(..),
    Divided(..),     
    Bidivisive(..)

) where
import Alpha.Canonical.Relations
import qualified Data.List as List

-- | Represents a family of types that support a notion of (potentially) heterogenous division
-- where a type instance is the type of the result of applying a conforming quotient operator
type family Divided a b     

-- / Characterizes a type that supports a notion of division
class Divisive a where
    -- | Divides the first operand by the second
    div::O2 a

    -- | Infix synonym for 'div'
    (/)::O2 a
    (/) = div
    {-# INLINE (/) #-}
    infixl 8 /    

-- | Characterizes pairs of types that support a notion of division
class Bidivisive a b where
    -- | Divides the first value by the second        
    bidiv::a -> b -> Divided a b

    -- | Infix synonym for 'hdiv'
    (>/<)::a -> b -> Divided a b
    (>/<) = bidiv
    {-# INLINE (>/<) #-}        
    infixl 8 >/<

-------------------------------------------------------------------------------
-- * Divisive instances
-------------------------------------------------------------------------------

instance Divisive Natural where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Integer where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Int where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Int8 where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Int16 where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Int32 where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Int64 where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Word where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Word8 where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Word16 where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Word32 where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Word64 where 
    div = quot'
    {-# INLINE div #-}
instance (Integral a) => Divisive (Ratio a) where 
    div = divf
    {-# INLINE div #-}    
instance Divisive Float where 
    div = divf
    {-# INLINE div #-}
instance Divisive Double where 
    div = divf
    {-# INLINE div #-}
instance Divisive CFloat where 
    div = divf
    {-# INLINE div #-}
instance Divisive CDouble where 
    div = divf
    {-# INLINE div #-}

