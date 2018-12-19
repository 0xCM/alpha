-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Subtractive
(    
    Subtractive(..), 
    Negatable(..),
    
) where
import Alpha.Base hiding(div)
import Alpha.Native
import Alpha.Canonical.Functions
import Alpha.Canonical.Elementary


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

data instance OpSpec 2 (Subtraction a) 
    = Subtraction (O2 a)

instance OpK f a => Operator 2 f a where
    type OpArgs 2 f a = (a,a)

    operator = Subtraction sub 
    {-# INLINE operator #-}        

    evaluate (a1,a2) =  f a1 a2 where (Subtraction f) = operator 
    {-# INLINE evaluate #-}        


-- | Characterizes types whose values are closed under 
-- additive negation
class Negatable a where
    -- | Negates the operand    
    negate::a -> a

newtype Negation a = Negative (O1 a)    

type OpN f a = (f ~ Negation a, Negatable a)
    
data instance OpSpec 1 (Negation a) 
    = Negation (O1 a)

instance OpN f a => Operator 1 f a where
    type OpArgs 1 f a = a

    operator = Negation negate
    {-# INLINE operator #-}        

    evaluate a = f a where
            (Negation f) = operator 
    {-# INLINE evaluate #-}        
    
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

-- Negatable 
-------------------------------------------------------------------------------
instance Negatable Integer where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int8 where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int16 where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int32 where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int64 where 
    negate = negate'
    {-# INLINE negate #-}
instance (Integral a) => Negatable (Ratio a) where 
    negate x = sub' 0 x
    {-# INLINE negate #-}    
instance Negatable Float where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Double where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable CFloat where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable CDouble where 
    negate = negate'
    {-# INLINE negate #-}


-- Subtractive tuples
-------------------------------------------------------------------------------
type Subtractive2 a1 a2 = (Subtractive a1, Subtractive a2)
type Subtractive3 a1 a2 a3 = (Subtractive2 a1 a2, Subtractive a3)
type Subtractive4 a1 a2 a3 a4 = (Subtractive3 a1 a2 a3, Subtractive a4)
type Subtractive5 a1 a2 a3 a4 a5 = (Subtractive4 a1 a2 a3 a4, Subtractive a5)

instance Subtractive2 a1 a2 => Subtractive (Tuple2 a1 a2) where
    sub (x1,x2) (y1,y2) = (x1 - y1, x2 - y2)

instance Subtractive3 a1 a2 a3 => Subtractive (Tuple3 a1 a2 a3) where
    sub  (x1,x2,x3) (y1,y2,y3) = (x1 - y1, x2 - y2,x3 - y3)

instance Subtractive4 a1 a2 a3 a4 => Subtractive (Tuple4 a1 a2 a3 a4) where
    sub  (x1,x2,x3,x4) (y1,y2,y3,y4) = (x1 - y1, x2 - y2,x3 - y3, x4 - y4)

instance Subtractive5 a1 a2 a3 a4 a5 => Subtractive (Tuple5 a1 a2 a3 a4 a5) where
    sub  (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) = (x1 - y1, x2 - y2,x3 - y3, x4 - y4,x5 - y5)

-- Negatable tuples
-------------------------------------------------------------------------------
type Negatable2 a1 a2 = (Negatable a1, Negatable a2)
type Negatable3 a1 a2 a3 = (Negatable2 a1 a2, Negatable a3)
type Negatable4 a1 a2 a3 a4 = (Negatable3 a1 a2 a3, Negatable a4)
type Negatable5 a1 a2 a3 a4 a5 = (Negatable4 a1 a2 a3 a4, Negatable a5)

instance Negatable2 a1 a2 => Negatable (Tuple2 a1 a2) where    
    negate (a1,a2) = (negate a1, negate a2)

instance Negatable3 a1 a2 a3 => Negatable (Tuple3 a1 a2 a3) where
    negate (a1,a2,a3) = (negate a1, negate a2, negate a3)

instance Negatable4 a1 a2 a3 a4 => Negatable (Tuple4 a1 a2 a3 a4) where
    negate (a1,a2,a3,a4) = (negate a1, negate a2, negate a3, negate a4)

instance Negatable5 a1 a2 a3 a4 a5  => Negatable (Tuple5 a1 a2 a3 a4 a5)  where
    negate (a1,a2,a3,a4,a5) = (negate a1, negate a2, negate a3, negate a4, negate a5)                