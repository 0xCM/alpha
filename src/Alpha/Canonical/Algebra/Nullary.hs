module Alpha.Canonical.Algebra.Nullary
(
    Nullary(..), null    
    

) where
import Alpha.Base hiding(div)
import Alpha.Canonical.Operators
import Alpha.Native


import qualified Data.List as List

-- Characterizes types that are inhabited by a canonical 0/empty value    
-- Note that there is no intent to link nullary and degenerate/degenerate values
-- although they will at times coincide
class Nullary a where
    -- Specifies the canonical 0 for an element relative to a structure
    zero::a

-- Tests whether a value is equal to the canonical zero
null::(Eq a, Nullary a)=>a -> Bool
null a = a == zero


instance Nullary [a] where
    zero = []

-- Nullary numbers
-------------------------------------------------------------------------------
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
