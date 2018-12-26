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
    Summation(..),summation,
    Addition(..), addition,

) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Common
import Alpha.Canonical.Collective.ItemSet

import qualified Data.Set as Set
import qualified Data.MultiSet as Bag
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Numeric.Interval as Interval

-- | Characterizes a type that supports a notion of  addition      
class Additive a where
    -- | Adds the first operand with the second
    add::O2 a
        
    -- | Infix synonym for 'add'    
    (+)::O2 a
    (+) = add
    {-# INLINE (+) #-}
    infixl 6 +

-- | Characterizes a type that supports the notion of additive identity
class Nullary a where
    -- | Specifies the unique element 0 such that 0 + a = a + 0 = a forall a
    zero::a

    -- Tests whether a value is equal to the canonical zero
    isZero::(Eq a) => a -> Bool
    isZero a = a == zero    
    
-- | Represents an addition operator
newtype Addition a = Addition (O2 a)    
    deriving(Generic)
instance Newtype (Addition a)

-- | Represents a formal sum
newtype Summation a = Summation [a]    

type instance Summed (ItemSet a) (ItemSet a) = ItemSet a    

-- | Produces the canonical addition operator
-- addition::(Num a) => Addition a
-- addition = Addition add'
addition::Additive a => Addition a
addition = Addition add

-- | Constructs a summation
summation::[a] -> Summation a
summation = Summation

instance Commutative (Addition a)
instance Associative (Addition a)
instance Additive a => BinaryOperator (Addition a) a where
    o2 = unwrap

instance (Nullary a, Additive a) => Computable (Summation a) where
    type Computed (Summation a) = a
    compute (Summation items) = reduce zero (+) items

    
instance (Ord a) =>  Additive (ItemSet a) where
    add x y = union x y
    {-# INLINE add #-}

instance (Ord a) => Nullary (ItemSet a) where
    zero = []

    
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