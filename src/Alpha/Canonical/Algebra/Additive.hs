module Alpha.Canonical.Algebra.Additive
(
    Additive(..),
    Summed(..), 
    Biadditive(..),    
    
) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations.Tuples

import qualified Data.Set as Set

-- | Represents a family of types that support a notion of (potentially) heterogenous addition
-- where a type instance is the addition result type
type family Summed a b

-- | Characterizes a type that supports a notion of  addition      
class Additive a where
    -- | Adds the first operand with the second
    add::a -> a -> a
        
    -- | Infix synonym for 'add'    
    (+)::a -> a -> a
    (+) = add
    {-# INLINE (+) #-}
    infixl 6 +

-- | Characterizes pairs of types that support a notion addition and
-- such addition need not be commutative so, in general,
-- hadd a + b != b + a
class Biadditive a b where
    -- | Adds the first operand with the second
    biadd::a -> b -> Summed a b

    -- | Infix synonym for 'hadd'
    (>+<)::a -> b -> Summed a b
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

type instance Summed Natural Natural = Natural
type instance Summed Integer Integer = Integer
type instance Summed Int Int = Int
type instance Summed Int8 Int8 = Int8
type instance Summed Int16 Int16 = Int16
type instance Summed Int32 Int32 = Int32
type instance Summed Int64 Int64 = Int64
type instance Summed Word Word = Word
type instance Summed Word8 Word8 = Word8
type instance Summed Word16 Word16 = Word16
type instance Summed Word32 Word32 = Word32
type instance Summed Word64 Word64 = Word64
type instance Summed (Ratio a) (Ratio a) = Ratio a
type instance Summed Float Float = Float
type instance Summed Double Double = Double
type instance Summed CFloat CFloat = CFloat
type instance Summed CDouble CDouble = CDouble
type instance Summed (ItemSet a) (ItemSet a) = ItemSet a

type UniSum a = Summed a a
type instance Summed (Tuple2 a1 a2) (Tuple2 a1 a2) = Tuple2 (UniSum a1) (UniSum a2)
type instance Summed (Tuple3 a1 a2 a3) (Tuple3 a1 a2 a3) = Tuple3 (UniSum a1) (UniSum a2) (UniSum a3)
type instance Summed (Tuple4 a1 a2 a3 a4) (Tuple4 a1 a2 a3 a4) = Tuple4 (UniSum a1) (UniSum a2) (UniSum a3) (UniSum a4)
type instance Summed (Tuple5 a1 a2 a3 a4 a5) (Tuple5 a1 a2 a3 a4 a5) = Tuple5 (UniSum a1) (UniSum a2) (UniSum a3) (UniSum a4) (UniSum a5)
    