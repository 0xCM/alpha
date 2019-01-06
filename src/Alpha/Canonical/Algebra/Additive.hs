-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Algebra.Additive
(
    Additive(..),
    Nullary(..),
    MultiSum(..),
    Summed(..),
    Biadditive(..),
    nsum,multisum,

) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Common

import qualified Data.Set as Set
import qualified Data.MultiSet as Bag
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Numeric.Interval as Interval

-- | Represents a family of types that support a notion of (potentially) heterogenous addition
-- where a type instance is the addition result type
type family Summed a b
type UniSum a = Summed a a
type instance Summed (Set a) (Set a) = Set a    

-- | Represents an addition operator
newtype Addition a = Addition (O2 a)    
    deriving(Generic)
instance Newtype (Addition a)

-- | Represents a formal sum of an arbitrary
-- number of elements
newtype MultiSum a = MultiSum [a]    


-- | Characterizes a type that supports a notion of  addition      
class Additive a where

    -- | Adds the first operand with the second
    add::O2 a
    add = (+)
    {-# INLINE add #-}

    -- | Infix synonym for 'add'    
    (+)::O2 a
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

-- | Characterizes a type that supports the notion of additive identity
class Nullary a where
    -- | Specifies the unique element 0 such that 0 + a = a + 0 = a forall a
    zero::a
    
-- | Constructs a summation
multisum::[a] -> MultiSum a
multisum = MultiSum

nsum::(Integral n, Nullary a, Additive a) => n -> a -> a
nsum n a = reduce zero (+) (clone n a)


-- | Produces the canonical addition operator
-- addition::(Num a) => Addition a
-- addition = Addition add'
addition::Additive a => Addition a
addition = Addition add

instance (Nullary a, Additive a) => Computable (MultiSum a) where
    type Computed (MultiSum a) = a
    compute (MultiSum items) = reduce zero (+) items

instance Commutative (Addition a)
instance Associative (Addition a)
    
instance (Ord a) =>  Additive (Set a) where
    add x y = union x y
    {-# INLINE add #-}

instance (Ord a) => Nullary (Set a) where
    zero = EmptySet

    
-- Additive numbers
-------------------------------------------------------------------------------
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

        
-- Nullary numbers
-------------------------------------------------------------------------------
instance Nullary Natural where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Integer where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Int where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Int8 where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Int16 where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Int32 where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Int64 where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Word where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Word8 where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Word16 where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Word32 where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Word64 where 
    zero = 0
    {-# INLINE zero #-}

instance (Integral a) => Nullary (Ratio a) where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Float where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary Double where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary CFloat where 
    zero = 0
    {-# INLINE zero #-}

instance Nullary CDouble where 
    zero = 0
    {-# INLINE zero #-}

-- Additive tuples
-------------------------------------------------------------------------------
type instance Individual (Tuple1 a1) = Tuple1 a1

instance Additive a => Additive (Tuple1 a) where
    add (Tuple1 x1) (Tuple1 y1) = Tuple1 (x1 + y1)

instance Additive a1 => Additive (UniTuple1 a1) where
    add (UniTuple1 x1) (UniTuple1 y1) = UniTuple1 (x1 + y1)
    
instance (Additive a1, Additive a2) => Additive (Tuple2 a1 a2) where
    add (x1,x2) (y1,y2) = (x1 + y1, x2 + y2)

instance (Additive a1, Additive a2, Additive a3) => Additive (Tuple3 a1 a2 a3) where
    add (x1,x2,x3) (y1,y2,y3) = (x1 + y1, x2 + y2,x3 + y3)

instance (Additive a1, Additive a2, Additive a3, Additive a4) => Additive (Tuple4 a1 a2 a3 a4) where
    add (x1,x2,x3,x4) (y1,y2,y3,y4) = (x1 + y1, x2 + y2,x3 + y3, x4 + y4)

instance (Additive a1, Additive a2, Additive a3, Additive a4, Additive a5) => Additive (Tuple5 a1 a2 a3 a4 a5) where
    add (x1,x2,x3,x4,x5) (y1,y2,y3,y4,y5) = (x1 + y1, x2 + y2,x3 + y3, x4 + y4,x5 + y5)
    
-- Nullary tuples
-------------------------------------------------------------------------------
type Nullary2 a1 a2 = (Nullary a1, Nullary a2)
type Nullary3 a1 a2 a3 = (Nullary2 a1 a2, Nullary a3)
type Nullary4 a1 a2 a3 a4 = (Nullary3 a1 a2 a3, Nullary a4)
type Nullary5 a1 a2 a3 a4 a5 = (Nullary4 a1 a2 a3 a4, Nullary a5)

instance Nullary a=> Nullary (UniTuple1 a) where
    zero = UniTuple1 (zero)
instance Nullary a=> Nullary (Tuple1 a) where
    zero = Tuple1 (zero)
    
instance Nullary2 a1 a2 => Nullary (Tuple2 a1 a2) where
    zero = (zero,zero)

instance Nullary3 a1 a2 a3 => Nullary (Tuple3 a1 a2 a3) where
    zero = (zero,zero,zero)

instance Nullary4 a1 a2 a3 a4 => Nullary (Tuple4 a1 a2 a3 a4) where
    zero = (zero,zero,zero,zero)

instance Nullary5 a1 a2 a3 a4 a5 => Nullary (Tuple5 a1 a2 a3 a4 a5) where
    zero = (zero,zero,zero,zero,zero)

type instance Summed (Tuple2 a1 a2) (Tuple2 a1 a2) = Tuple2 (UniSum a1) (UniSum a2)
type instance Summed (Tuple3 a1 a2 a3) (Tuple3 a1 a2 a3) = Tuple3 (UniSum a1) (UniSum a2) (UniSum a3)
type instance Summed (Tuple4 a1 a2 a3 a4) (Tuple4 a1 a2 a3 a4) = Tuple4 (UniSum a1) (UniSum a2) (UniSum a3) (UniSum a4)
type instance Summed (Tuple5 a1 a2 a3 a4 a5) (Tuple5 a1 a2 a3 a4 a5) = Tuple5 (UniSum a1) (UniSum a2) (UniSum a3) (UniSum a4) (UniSum a5)
    