module Alpha.Canonical.Algebra.Divisive
(
    Quotient(..), Divisive(..), Bidivisive(..),     
) where
import Alpha.Base hiding(div)
import Alpha.Native
import Alpha.Canonical.Relations
import Alpha.Canonical.Operators
import qualified Data.List as List

-- | Represents a family of types that support a notion of (potentially) heterogenous division
-- where a type instance is the type of the result of applying a conforming quotient operator
type family Quotient a b    

-- Homogenous Quotient type
-------------------------------------------------------------------------------
type instance Quotient Int Int = Int
type instance Quotient Int8 Int8 = Int8
type instance Quotient Int16 Int16 = Int16
type instance Quotient Int32 Int32 = Int32
type instance Quotient Int64 Int64 = Int64
type instance Quotient Integer Integer = Integer
type instance Quotient Word Word = Word
type instance Quotient Word8 Word8 = Word8
type instance Quotient Word16 Word16 = Word16
type instance Quotient Word32 Word32 = Word32
type instance Quotient Word64 Word64 = Word64
type instance Quotient Natural Natural = Natural
type instance Quotient (Ratio a) (Ratio a) = Ratio a
type instance Quotient Float Float = Float
type instance Quotient Double Double = Double
type instance Quotient CFloat CFloat = CFloat
type instance Quotient CDouble CDouble = CDouble

-- / Characterizes a type that supports a notion of division
class Divisive a where
    -- | Divides the first operand by the second
    div::BinaryOperator a

    -- | Infix synonym for 'div'
    (/)::BinaryOperator a
    (/) = div
    {-# INLINE (/) #-}
    infixl 8 /    

-- | Characterizes pairs of types that support a notion of division
class Bidivisive a b where
    -- | Divides the first value by the second        
    bidiv::a -> b -> Quotient a b

    -- | Infix synonym for 'hdiv'
    (>/<)::a -> b -> Quotient a b
    (>/<) = bidiv
    {-# INLINE (>/<) #-}        
    infixl 8 >/<

instance Divisive Natural where 
    div = quot
    {-# INLINE div #-}
instance Divisive Integer where 
    div = quot
    {-# INLINE div #-}
instance Divisive Int where 
    div = quot
    {-# INLINE div #-}
instance Divisive Int8 where 
    div = quot
    {-# INLINE div #-}
instance Divisive Int16 where 
    div = quot
    {-# INLINE div #-}
instance Divisive Int32 where 
    div = quot
    {-# INLINE div #-}
instance Divisive Int64 where 
    div = quot
    {-# INLINE div #-}
instance Divisive Word where 
    div = quot
    {-# INLINE div #-}
instance Divisive Word8 where 
    div = quot
    {-# INLINE div #-}
instance Divisive Word16 where 
    div = quot
    {-# INLINE div #-}
instance Divisive Word32 where 
    div = quot
    {-# INLINE div #-}
instance Divisive Word64 where 
    div = quot
    {-# INLINE div #-}
instance (Integral a) => Divisive (Ratio a) where 
    div = divf'
    {-# INLINE div #-}    
instance Divisive Float where 
    div = divf'
    {-# INLINE div #-}
instance Divisive Double where 
    div = divf'
    {-# INLINE div #-}
instance Divisive CFloat where 
    div = divf'
    {-# INLINE div #-}
instance Divisive CDouble where 
    div = divf'
    {-# INLINE div #-}

-- HDivisive
-------------------------------------------------------------------------------
instance Bidivisive Natural Natural where 
    bidiv = div'
    {-# INLINE bidiv #-}
instance Bidivisive Integer Integer where 
    bidiv = div'
    {-# INLINE bidiv #-}
instance Bidivisive Int Int where 
    bidiv = div'
    {-# INLINE bidiv #-}
instance Bidivisive Int8 Int8 where 
    bidiv = div'
    {-# INLINE bidiv #-}
instance Bidivisive Int16 Int16 where 
    bidiv = div'
    {-# INLINE bidiv #-}
instance Bidivisive Int32 Int32 where 
    bidiv = div'
    {-# INLINE bidiv #-}
instance Bidivisive Int64 Int64 where 
    bidiv = div'
    {-# INLINE bidiv #-}
instance Bidivisive Word Word where 
    bidiv = div'
    {-# INLINE bidiv #-}
instance Bidivisive Word8 Word8 where 
    bidiv = div'
    {-# INLINE bidiv #-}
instance Bidivisive Word16 Word16 where 
    bidiv = div'
    {-# INLINE bidiv #-}
instance Bidivisive Word32 Word32 where 
    bidiv = div'
    {-# INLINE bidiv #-}
instance Bidivisive Word64 Word64 where 
    bidiv = div'
    {-# INLINE bidiv #-}
instance (Integral a) => Bidivisive (Ratio a) (Ratio a) where 
    bidiv = divf'    
    {-# INLINE bidiv #-}
instance Bidivisive Float Float where 
    bidiv = divf'
    {-# INLINE bidiv #-}
instance Bidivisive Double Double where 
    bidiv = divf'
    {-# INLINE bidiv #-}
instance Bidivisive CFloat CFloat where 
    bidiv = divf'
    {-# INLINE bidiv #-}
instance Bidivisive CDouble CDouble where 
    bidiv = divf'
    {-# INLINE bidiv #-}
