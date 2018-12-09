module Alpha.Canonical.Algebra.Divisive
(
    Quotient(..), Divisive(..), HDivisive(..), 
    

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
class HDivisive a b where
    -- | Divides the first value by the second        
    hdiv::a -> b -> Quotient a b

    -- | Infix synonym for 'hdiv'
    (>/<)::a -> b -> Quotient a b
    (>/<) = hdiv
    {-# INLINE (>/<) #-}        
    infixl 8 >/<

instance Divisive Natural where 
    div = div'
    {-# INLINE div #-}
instance Divisive Integer where 
    div = div'
    {-# INLINE div #-}
instance Divisive Int where 
    div = div'
    {-# INLINE div #-}
instance Divisive Int8 where 
    div = div'
    {-# INLINE div #-}
instance Divisive Int16 where 
    div = div'
    {-# INLINE div #-}
instance Divisive Int32 where 
    div = div'
    {-# INLINE div #-}
instance Divisive Int64 where 
    div = div'
    {-# INLINE div #-}
instance Divisive Word where 
    div = div'
    {-# INLINE div #-}
instance Divisive Word8 where 
    div = div'
    {-# INLINE div #-}
instance Divisive Word16 where 
    div = div'
    {-# INLINE div #-}
instance Divisive Word32 where 
    div = div'
    {-# INLINE div #-}
instance Divisive Word64 where 
    div = div'
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
instance HDivisive Natural Natural where 
    hdiv = div'
    {-# INLINE hdiv #-}
instance HDivisive Integer Integer where 
    hdiv = div'
    {-# INLINE hdiv #-}
instance HDivisive Int Int where 
    hdiv = div'
    {-# INLINE hdiv #-}
instance HDivisive Int8 Int8 where 
    hdiv = div'
    {-# INLINE hdiv #-}
instance HDivisive Int16 Int16 where 
    hdiv = div'
    {-# INLINE hdiv #-}
instance HDivisive Int32 Int32 where 
    hdiv = div'
    {-# INLINE hdiv #-}
instance HDivisive Int64 Int64 where 
    hdiv = div'
    {-# INLINE hdiv #-}
instance HDivisive Word Word where 
    hdiv = div'
    {-# INLINE hdiv #-}
instance HDivisive Word8 Word8 where 
    hdiv = div'
    {-# INLINE hdiv #-}
instance HDivisive Word16 Word16 where 
    hdiv = div'
    {-# INLINE hdiv #-}
instance HDivisive Word32 Word32 where 
    hdiv = div'
    {-# INLINE hdiv #-}
instance HDivisive Word64 Word64 where 
    hdiv = div'
    {-# INLINE hdiv #-}
instance (Integral a) => HDivisive (Ratio a) (Ratio a) where 
    hdiv = divf'    
    {-# INLINE hdiv #-}
instance HDivisive Float Float where 
    hdiv = divf'
    {-# INLINE hdiv #-}
instance HDivisive Double Double where 
    hdiv = divf'
    {-# INLINE hdiv #-}
instance HDivisive CFloat CFloat where 
    hdiv = divf'
    {-# INLINE hdiv #-}
instance HDivisive CDouble CDouble where 
    hdiv = divf'
    {-# INLINE hdiv #-}
