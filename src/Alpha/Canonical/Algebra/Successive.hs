-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Algebra.Successive
(
    Incrementable(..), 
    Decrementable(..), 
    Successive(..), 
    Antecedent(..),
    Bidecrementable(..),
    Increment(..),
    Biincrementable(..),
    Decrement(..),
    
) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Subtractive
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Multiplicative
import qualified Data.List as List

type family Increment a 
type family Decrement a 

class Bidecrementable a where
    bidec::a -> Decrement a

class Biincrementable a where
    biinc::a -> Increment a

-- / Characterizes a type that can be incremented an indefinite number of times
-- but, depending on the realization, may eventually cycle
class Incrementable a where
    -- Increments the operand by one unit
    inc::a -> a
    default inc::(Additive a, Unital a) => a -> a
    inc a = a + one
    {-# INLINE inc #-}


-- / Characterizes a type that can be decremented an indefinite number of times
-- but, depending on the realization, may eventually cycle
class Decrementable a where
    -- Decrements the operand by one unit
    dec::a -> a 
    default dec::(Subtractive a, Unital a) => a -> a
    dec a = a - one
    {-# INLINE dec #-}


class (Incrementable a, Decrementable a) => Alternating a where
instance (Incrementable a, Decrementable a) => Alternating a 

-- / Characterizes a type with which a strictly monotonic finite sequence 
-- of ascending values is associated
class Successive a where
    next::a -> Maybe a

-- / Characterizes a type with which a strictly monotonic finite sequence 
-- of descending values is associated
class Antecedent a where    
    prior::a -> Maybe a


-- Decrement class intsances
-------------------------------------------------------------------------------
instance Decrementable Int
instance Decrementable Int8
instance Decrementable Int16
instance Decrementable Int32
instance Decrementable Int64
instance Decrementable Word
instance Decrementable Word8
instance Decrementable Word16
instance Decrementable Word32
instance Decrementable Word64
instance Decrementable Natural
instance Decrementable Integer
instance (Integral a, Ord a) => Decrementable (Ratio a)
instance Decrementable Float
instance Decrementable Double
instance Decrementable CFloat
instance Decrementable CDouble
        

-- Incrementable class intsances
-------------------------------------------------------------------------------
instance Incrementable Int
instance Incrementable Int8
instance Incrementable Int16
instance Incrementable Int32
instance Incrementable Int64
instance Incrementable Word
instance Incrementable Word8
instance Incrementable Word16
instance Incrementable Word32
instance Incrementable Word64
instance Incrementable Natural
instance Incrementable Integer
instance (Integral a, Ord a) => Incrementable (Ratio a)
instance Incrementable Float
instance Incrementable Double
instance Incrementable CFloat
instance Incrementable CDouble

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