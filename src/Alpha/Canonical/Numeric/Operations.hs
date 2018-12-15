module Alpha.Canonical.Numeric.Operations
where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Operators
import Alpha.Canonical.Common
import Alpha.Canonical.Element
import Alpha.Canonical.Relations.Comparison
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Subtractive
import Alpha.Canonical.Algebra.Negatable

import Alpha.Canonical.Numeric.Types

    
instance Absolute Natural where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolute Integer where 
    abs = fromIntegral.abs'
    {-# INLINE abs #-}
instance Absolute Int where 
    abs = fromIntegral.abs'
    {-# INLINE abs #-}
instance Absolute Int8 where 
    abs = fromIntegral.abs'
    {-# INLINE abs #-}
instance Absolute Int16 where 
    abs = fromIntegral.abs'
    {-# INLINE abs #-}
instance Absolute Int32 where 
    abs = fromIntegral.abs'
    {-# INLINE abs #-}
instance Absolute Int64 where 
    abs = fromIntegral.abs'
    {-# INLINE abs #-}
instance Absolute Word where 
    abs = abs'
    {-# INLINE abs #-}    
instance Absolute Word8 where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolute Word16 where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolute Word32 where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolute Word64 where 
    abs = abs'
    {-# INLINE abs #-}
instance (Integral a) => Absolute (Ratio a) where
    abs = abs'
instance Absolute Float where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolute Double where 
    abs = abs'
    {-# INLINE abs #-}
instance Absolute CFloat where 
    abs =  abs'     
    {-# INLINE abs #-}
instance Absolute CDouble where 
    abs  = abs'
    {-# INLINE abs #-}
instance Absolute UFloat where 
    abs = id
    {-# INLINE abs #-}
instance Absolute UDouble where 
    abs = id
    {-# INLINE abs #-}    
instance Absolute UCFloat where 
    abs = id
    {-# INLINE abs #-}
instance Absolute UCDouble where 
    abs = id
    {-# INLINE abs #-}

nextB::(BoundedIntegral n,Additive n) => n -> Maybe n
nextB n = ifelse (n == maxBound) none (just (n + 1))

priorB::(BoundedIntegral n,Subtractive n) => n -> Maybe n
priorB n = ifelse (n == minBound) none (just (n - 1))
    

instance Enumerable Integer where
    next n = just (n + 1)
    prior n = just (n - 1)

instance Enumerable Natural where
    next n = just (n + 1)
    prior n = ifelse (n > 0) (just(n - 1)) none
    
instance Integral n => Enumerable (Ratio n) where
    next n = just (n + 1)
    prior n = ifelse (n > 0) (just(n - 1)) none
        
instance Enumerable Word where
    next = nextB
    prior = priorB

instance Enumerable Word8 where
    next = nextB
    prior = priorB

instance Enumerable Word16 where
    next = nextB
    prior = priorB

instance Enumerable Word32 where
    next = nextB
    prior = priorB

instance Enumerable Word64 where
    next = nextB
    prior = priorB

instance Enumerable Int where
    next = nextB
    prior = priorB

instance Enumerable Int8 where
    next = nextB
    prior = priorB

instance Enumerable Int16 where
    next = nextB
    prior = priorB

instance Enumerable Int32 where
    next = nextB
    prior = priorB

instance Enumerable Int64 where
    next = nextB
    prior = priorB

instance InvariantSet Int where
    invariants =  [minBound .. maxBound]                
instance InvariantSet Word where
    invariants =  [minBound .. maxBound]
instance InvariantSet Word8 where
    invariants =  [minBound .. maxBound]
instance InvariantSet Word16 where
    invariants =  [minBound .. maxBound]
instance InvariantSet Word32 where
    invariants =  [minBound .. maxBound]
instance InvariantSet Word64 where
    invariants =  [minBound .. maxBound]
instance InvariantSet Int8 where
    invariants =  [minBound .. maxBound]
instance InvariantSet Int16 where
    invariants =  [minBound .. maxBound]
instance InvariantSet Int32 where
    invariants =  [minBound .. maxBound]
instance InvariantSet Int64 where
    invariants =  [minBound .. maxBound]                                    
instance InvariantSet Natural where
    invariants =  [0 .. ]

-- instance InvariantSet Integer where
--     invariants =  [x | ] where

--         upper = [1::Integer ..]
--         lower = negate <$> upper
        