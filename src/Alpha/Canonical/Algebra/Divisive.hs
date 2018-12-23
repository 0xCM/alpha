-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Divisive
(
    Divisive(..),
    IntegralDivision(..), divisionI,
    FloatingDivision(..), divisionF,

) where
import Alpha.Canonical.Relations
import qualified Data.List as List


-- / Characterizes a type that supports a notion of division
class Divisive a where
    -- | Divides the first operand by the second
    div::O2 a

    -- | Infix synonym for 'div'
    (/)::O2 a
    (/) = div
    {-# INLINE (/) #-}
    infixl 8 /    

-- | Represents a Euclidean division operator
newtype IntegralDivision a = IntegralDivision (O2 a)    
    deriving(Generic)
instance Newtype (IntegralDivision a)

-- | Produces the canonical Euclidean division operator
divisionI::(Integral a, Divisive a) => IntegralDivision a
divisionI = IntegralDivision div

instance (Integral a, Divisive a) => Operator (IntegralDivision a) where
    type Operand (IntegralDivision a) = a

    operator = divisionI
    {-# INLINE operator #-}


instance (Integral a, Divisive a) => BinaryOperator (IntegralDivision a) where
    evaluate (IntegralDivision f) (a1,a2) = f a1 a2
    {-# INLINE evaluate #-}

-- Floating Division
-------------------------------------------------------------------------------

-- | Represents a floating division operator
newtype FloatingDivision a = FloatingDivision (O2 a)    
    deriving(Generic)
instance Newtype (FloatingDivision a)


-- | Produces the canonical floating division operator    
divisionF::(Divisive a, Floating a) => FloatingDivision a
divisionF = FloatingDivision $ div

instance (Divisive a, Floating a) => Operator (FloatingDivision a) where
    type Operand (FloatingDivision a) = a

    operator = divisionF
    {-# INLINE operator #-}

instance (Divisive a, Floating a) => BinaryOperator (FloatingDivision a) where
    evaluate (FloatingDivision f) (a1,a2) = f a1 a2
    {-# INLINE evaluate #-}


instance Divisive Natural where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Integer where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Int where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Int8 where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Int16 where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Int32 where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Int64 where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Word where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Word8 where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Word16 where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Word32 where 
    div = quot'
    {-# INLINE div #-}
instance Divisive Word64 where 
    div = quot'
    {-# INLINE div #-}
instance (Integral a) => Divisive (Ratio a) where 
    div = divf
    {-# INLINE div #-}    
instance Divisive Float where 
    div = divf
    {-# INLINE div #-}
instance Divisive Double where 
    div = divf
    {-# INLINE div #-}
instance Divisive CFloat where 
    div = divf
    {-# INLINE div #-}
instance Divisive CDouble where 
    div = divf
    {-# INLINE div #-}

