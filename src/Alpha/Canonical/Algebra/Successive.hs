-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Successive
(
    Increment(..), Incrementable(..),
    Decrement(..), Decrementable(..),
    
) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Subtractive
import Alpha.Canonical.Algebra.Additive
import qualified Data.List as List

-- | Represents a family of types that support some sort of incrementing operation where
-- a type instance is the type of an incremented type. The canonical example
-- is a successor function in the context of type-level naturals
type family Increment a 

type instance Increment Int = Int
type instance Increment Int8 = Int8
type instance Increment Int16 = Int16
type instance Increment Int32 = Int32
type instance Increment Int64 = Int64
type instance Increment Integer = Integer
type instance Increment Word = Word
type instance Increment Word8 = Word8
type instance Increment Word16 = Word16
type instance Increment Word32 = Word32
type instance Increment Word64 = Word64
type instance Increment Natural = Natural
type instance Increment (Ratio a) = Ratio a
type instance Increment Float = Float
type instance Increment Double = Double
type instance Increment CFloat = CFloat
type instance Increment CDouble = CDouble

-- | Represents a family of types that support some sort of decrementing operation where
-- a type instance is the type of an incremented type. The canonical example
-- is a successor function in the context of type-level naturals
type family Decrement a 

type instance Decrement Int = Int
type instance Decrement Int8 = Int8
type instance Decrement Int16 = Int16
type instance Decrement Int32 = Int32
type instance Decrement Int64 = Int64
type instance Decrement Integer = Integer
type instance Decrement Word = Word
type instance Decrement Word8 = Word8
type instance Decrement Word16 = Word16
type instance Decrement Word32 = Word32
type instance Decrement Word64 = Word64
type instance Decrement Natural = Natural
type instance Decrement (Ratio a) = Ratio a
type instance Decrement Float = Float
type instance Decrement Double = Double
type instance Decrement CFloat = CFloat
type instance Decrement CDouble = CDouble


class Incrementable a where
    -- Increments the operand by one unit
    inc::a -> Increment a 

    -- | Infix synonym for 'inc'
    (>++<)::a -> Increment a
    (>++<) = inc
    {-# INLINE (>++<) #-}
    infix 2 >++<

class Decrementable a where
    -- Decrements the operand by one unit
    dec::a -> Decrement a 

    -- | Infix synonym for 'inc'
    (>--<)::a -> Decrement a
    (>--<) = dec
    {-# INLINE (>--<) #-}
    infix 2 >--<


-- Decrement class intsances
-------------------------------------------------------------------------------
instance Decrementable Int where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable Int8 where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable Int16 where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable Int32 where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable Int64 where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable Word where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable Word8 where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable Word16 where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable Word32 where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable Word64 where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable Natural where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable Integer where
    dec = sub' 1
    {-# INLINE dec #-}            
instance (Integral a, Ord a) => Decrementable (Ratio a) where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable Float where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable Double where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable CFloat where
    dec = sub' 1
    {-# INLINE dec #-}
instance Decrementable CDouble where
    dec = sub' 1
    {-# INLINE dec #-}
        

-- Incrementable class intsances
-------------------------------------------------------------------------------
instance Incrementable Int where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable Int8 where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable Int16 where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable Int32 where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable Int64 where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable Word where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable Word8 where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable Word16 where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable Word32 where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable Word64 where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable Natural where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable Integer where
    inc = add' 1
    {-# INLINE inc #-}            
instance (Integral a, Ord a) => Incrementable (Ratio a) where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable Float where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable Double where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable CFloat where
    inc = add' 1
    {-# INLINE inc #-}
instance Incrementable CDouble where
    inc = add' 1
    {-# INLINE inc #-}

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

