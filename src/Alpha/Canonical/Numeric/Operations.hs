module Alpha.Canonical.Numeric.Operations
where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Operators
import Alpha.Canonical.Common
import Alpha.Canonical.Element
import Alpha.Canonical.Relations
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

-- Successive
-------------------------------------------------------------------------------
instance Successive Integer where
    next n = just (n + 1)

instance Successive Natural where
    next n = just (n + 1)
    
instance Integral n => Successive (Ratio n) where
    next n = just (n + 1)
        
instance Successive Word where
    next n = ifelse (n == maxBound) none (just (n + 1))

instance Successive Word8 where
    next n = ifelse (n == maxBound) none (just (n + 1))

instance Successive Word16 where
    next n = ifelse (n == maxBound) none (just (n + 1))

instance Successive Word32 where
    next n = ifelse (n == maxBound) none (just (n + 1))

instance Successive Word64 where
    next n = ifelse (n == maxBound) none (just (n + 1))

instance Successive Int where
    next n = ifelse (n == maxBound) none (just (n + 1))

instance Successive Int8 where
    next n = ifelse (n == maxBound) none (just (n + 1))

instance Successive Int16 where
    next n = ifelse (n == maxBound) none (just (n + 1))

instance Successive Int32 where
    next n = ifelse (n == maxBound) none (just (n + 1))

instance Successive Int64 where
    next n = ifelse (n == maxBound) none (just (n + 1))
        
-- Antecedent
-------------------------------------------------------------------------------
instance Antecedent Integer where
    prior n = just (n - 1)

instance Antecedent Natural where
    prior n = ifelse (n > 0) (just(n - 1)) none
    
instance Integral n => Antecedent (Ratio n) where
    prior n = ifelse (n > 0) (just(n - 1)) none
        
instance Antecedent Word where
    prior n = ifelse (n == minBound) none (just (n - 1))

instance Antecedent Word8 where
    prior n = ifelse (n == minBound) none (just (n - 1))

instance Antecedent Word16 where
    prior n = ifelse (n == minBound) none (just (n - 1))

instance Antecedent Word32 where
    prior n = ifelse (n == minBound) none (just (n - 1))

instance Antecedent Word64 where
    prior n = ifelse (n == minBound) none (just (n - 1))

instance Antecedent Int where
    prior n = ifelse (n == minBound) none (just (n - 1))

instance Antecedent Int8 where
    prior n = ifelse (n == minBound) none (just (n - 1))

instance Antecedent Int16 where
    prior n = ifelse (n == minBound) none (just (n - 1))

instance Antecedent Int32 where
    prior n = ifelse (n == minBound) none (just (n - 1))

instance Antecedent Int64 where
    prior n = ifelse (n == minBound) none (just (n - 1))

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

