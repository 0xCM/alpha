-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Subtractive
(    
    Subtractive(..), 
    Subtraction(..), subtraction,
    Subtracted(..), 
    Bisubtractive(..),
    
) where
import Alpha.Canonical.Relations

-- | Represents a family of types that support a notion of (potentially) heterogeneous 
-- subtraction where the instance type is the result type of applying a 
-- conforming subtraction operation
type family Subtracted a b


-- / Characterizes a type that supports a notion of subtraction
class Subtractive a where
    -- | Subracts the second value from the first
    sub::O2 a
    sub = (-)
    {-# INLINE sub #-}

    -- | Infix synonym for 'sub'    
    (-)::O2 a
    (-) = sub
    {-# INLINE (-) #-}
    infixl 6 -    
    
-- / Characterizes a pair of type that supports a notion of heterogenious subtraction
class Bisubtractive a b where
    -- | Calculates the difference between the first value and the second
    bisub::a -> b -> Subtracted a b

    -- | Infix synonym for 'hsub'        
    (>-<)::a -> b -> Subtracted a b
    (>-<) = bisub
    infixl 6 >-<    

-- | Represents a subtraction operator
newtype Subtraction a = Subtraction (O2 a)    
    deriving(Generic)
instance Newtype (Subtraction a)

-- | Produces the canonical subtraction operator
subtraction::(Subtractive a) => Subtraction a
subtraction = Subtraction sub


instance Associative (Subtraction a)


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



-- Subtractive tuples
-------------------------------------------------------------------------------
type Subtractive2 a1 a2 = (Subtractive a1, Subtractive a2)
type Subtractive3 a1 a2 a3 = (Subtractive2 a1 a2, Subtractive a3)
type Subtractive4 a1 a2 a3 a4 = (Subtractive3 a1 a2 a3, Subtractive a4)
type Subtractive5 a1 a2 a3 a4 a5 = (Subtractive4 a1 a2 a3 a4, Subtractive a5)

instance Subtractive a => Subtractive (UniTuple1 a) where
    sub (UniTuple1 x) (UniTuple1 y) = UniTuple1 (x - y)

instance Subtractive2 a1 a2 => Subtractive (Tuple2 a1 a2) where
    sub (x1,x2) (y1,y2) = (x1 - y1, x2 - y2)

instance Subtractive3 a1 a2 a3 => Subtractive (Tuple3 a1 a2 a3) where
    sub  (x1,x2,x3) (y1,y2,y3) = (x1 - y1, x2 - y2,x3 - y3)

instance Subtractive4 a1 a2 a3 a4 => Subtractive (Tuple4 a1 a2 a3 a4) where
    sub  (x1,x2,x3,x4) (y1,y2,y3,y4) = (x1 - y1, x2 - y2,x3 - y3, x4 - y4)

instance Subtractive5 a1 a2 a3 a4 a5 => Subtractive (Tuple5 a1 a2 a3 a4 a5) where
    sub  (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) = (x1 - y1, x2 - y2,x3 - y3, x4 - y4,x5 - y5)

