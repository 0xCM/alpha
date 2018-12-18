module Alpha.Canonical.Algebra.Subtractive
(
    
    Subtractive(..), 
    Subtracted(..), 
    Bisubtractive(..)
    
) where
import Alpha.Base hiding(div)
import Alpha.Canonical.Operators
import Alpha.Native

-- | Represents a family of types that support a notion of (potentially) heterogeneous 
-- subtraction where the instance type is the result type of applying a 
-- conforming subtraction operation
type family Subtracted a b

-- / Characterizes a type that supports a notion of subtraction
class Subtractive a where
    -- | Subracts the second value from the first
    sub::O2 a

    -- | Infix synonym for 'sub'    
    (-)::O2 a
    (-) = sub
    {-# INLINE (-) #-}
    infixl 6 -    

newtype Subtraction a = Subtractive (O2 a)
    deriving(Generic)
instance Newtype (Subtraction a)        
type OpK f a = (f ~ Subtraction a, Subtractive a)

data instance Operator 2 (Subtraction a) 
    = Subtraction (O2 a)

instance OpK f a => Operative 2 f a where
    type OpArgs 2 f a = (a,a)

    operator = Subtraction sub 
    {-# INLINE operator #-}        

    evaluate (a1,a2) =  f a1 a2 where (Subtraction f) = operator 
    {-# INLINE evaluate #-}        

-- / Characterizes a pair of type that supports a notion of heterogenious subtraction
class Bisubtractive a b where
    -- | Calculates the difference between the first value and the second
    bisub::a -> b -> Subtracted a b

    -- | Infix synonym for 'hsub'        
    (>-<)::a -> b -> Subtracted a b
    (>-<) = bisub
    infixl 6 >-<    

    
-- Subtractive
-------------------------------------------------------------------------------
instance Subtractive Natural where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Integer where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int8 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int16 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int32 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Int64 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word8 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word16 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word32 where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Word64 where 
    sub = sub'
    {-# INLINE sub #-}
instance (Integral a) => Subtractive (Ratio a) where 
    sub = sub'
    {-# INLINE sub #-}    
instance Subtractive Float where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive Double where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive CFloat where 
    sub = sub'
    {-# INLINE sub #-}
instance Subtractive CDouble where 
    sub = sub'
    {-# INLINE sub #-}

-- type instance Subtracted Natural Natural = Natural
-- type instance Subtracted Integer Integer = Integer
-- type instance Subtracted Int Int = Int
-- type instance Subtracted Int8 Int8 = Int8
-- type instance Subtracted Int16 Int16 = Int16
-- type instance Subtracted Int32 Int32 = Int32
-- type instance Subtracted Int64 Int64 = Int64
-- type instance Subtracted Word Word = Word
-- type instance Subtracted Word8 Word8 = Word8
-- type instance Subtracted Word16 Word16 = Word16
-- type instance Subtracted Word32 Word32 = Word32
-- type instance Subtracted Word64 Word64 = Word64
-- type instance Subtracted (Ratio a) (Ratio a) = Ratio a
-- type instance Subtracted Float Float = Float
-- type instance Subtracted Double Double = Double
-- type instance Subtracted CFloat CFloat = CFloat
-- type instance Subtracted CDouble CDouble = CDouble
    