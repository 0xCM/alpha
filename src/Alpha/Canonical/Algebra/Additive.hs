module Alpha.Canonical.Algebra.Additive
(
    Bisum(..), Additive(..), Biadditive(..),    
    
) where
import Alpha.Base
import Alpha.Canonical.Operators
import Alpha.Native

import qualified Data.Set as Set

-- | Represents a family of types that support a notion of (potentially) heterogenous addition
-- where a type instance is the addition result type
type family Bisum a b

-- | Characterizes a type that supports a notion of  *commutative* addition      
-- Thus, the following invariant must hold:
-- forall a b => add a b = add b a
class Additive a where
    -- | Adds the first operand with the second
    add::BinaryOperator a
    
    
    -- | Infix synonym for 'add'    
    (+)::BinaryOperator a
    (+) = add
    {-# INLINE (+) #-}
    infixl 6 +

-- | Characterizes pairs of types that support a notion addition and
-- such addition need not be commutative so, in general,
-- hadd a + b != b + a
class Biadditive a b where
    -- | Adds the first operand with the second
    biadd::a -> b -> Bisum a b

    -- | Infix synonym for 'hadd'
    (>+<)::a -> b -> Bisum a b
    (>+<) = biadd
    {-# INLINE (>+<) #-}
    infixl 6 >+<

instance (Ord a) =>  Additive (ItemSet a) where
    add = union'
    {-# INLINE add #-}

instance Additive Natural where 
    add = add'
    {-# INLINE add #-}

instance Additive Integer where 
    add = add'
    {-# INLINE add #-}

instance Additive Int where 
    add = add'
    {-# INLINE add #-}

instance Additive Int8 where 
    add = add'
    {-# INLINE add #-}

instance Additive Int16 where 
    add = add'
    {-# INLINE add #-}

instance Additive Int32 where 
    add = add'
    {-# INLINE add #-}

instance Additive Int64 where 
    add = add'
    {-# INLINE add #-}

instance Additive Word where 
    add = add'
    {-# INLINE add #-}

instance Additive Word8 where 
    add = add'
    {-# INLINE add #-}

instance Additive Word16 where 
    add = add'
    {-# INLINE add #-}

instance Additive Word32 where 
    add = add'
    {-# INLINE add #-}

instance Additive Word64 where 
    add = add'
    {-# INLINE add #-}

instance (Integral a) => Additive (Ratio a) where 
    add = add'
    {-# INLINE add #-}    

instance Additive Float where 
    add = add'
    {-# INLINE add #-}

instance Additive Double where 
    add = add'
    {-# INLINE add #-}

instance Additive CFloat where 
    add = add'
    {-# INLINE add #-}

instance Additive CDouble where 
    add = add'
    {-# INLINE add #-}

    
-- Additive tuples
-------------------------------------------------------------------------------
type Additive2 a1 a2 = (Additive a1, Additive a2)
type Additive3 a1 a2 a3 = (Additive2 a1 a2, Additive a3)
type Additive4 a1 a2 a3 a4 = (Additive3 a1 a2 a3, Additive a4)
type Additive5 a1 a2 a3 a4 a5 = (Additive4 a1 a2 a3 a4, Additive a5)

instance Additive2 a1 a2 => Additive (a1,a2) where
    add (x1,x2) (y1,y2) = (x1 + y1, x2 + y2)

instance Additive3 a1 a2 a3 => Additive (a1,a2,a3) where
    add (x1,x2,x3) (y1,y2,y3) = (x1 + y1, x2 + y2,x3 + y3)

instance Additive4 a1 a2 a3 a4 => Additive (a1,a2,a3,a4) where
    add (x1,x2,x3,x4) (y1,y2,y3,y4) = (x1 + y1, x2 + y2,x3 + y3, x4 + y4)

instance Additive5 a1 a2 a3 a4 a5 => Additive (a1,a2,a3,a4,a5) where
    add (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) = (x1 + y1, x2 + y2,x3 + y3, x4 + y4,x5 + y5)
