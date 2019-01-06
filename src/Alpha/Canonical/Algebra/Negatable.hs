-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Negatable
(    
    Negatable(..),
    Negation(..), 
    Negated(..),
    Binegatable(..),

    alternate
) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Collective
import Alpha.Canonical.Algebra.Subtractive
import qualified Data.List as List

-- Defines a family of types that represent the result of applying a
-- (potentially) heterogeneous negation operation
type family Negated a


-- | Characterizes types whose values are closed under 
-- additive negation
class Subtractive a => Negatable a where
    -- | Negates the operand    
    negate::a -> a

-- / Characterizes types for which unary negation is defined
class Binegatable a where
    -- | Negates the operand    
    binegate::a -> Negated a

-- | Represents a negation operator
newtype Negation a = Negation (O1 a)    
    deriving(Generic)
instance Newtype (Negation a)

-- | Produces the canonical negation operator
negation::Negatable a => Negation a
negation = Negation negate

-- | Accepts a list of negatable things, creates
-- a new list by negating each element in the
-- input list and returns the admixture vis-a-vis
-- 'intermix'
alternate::(Negatable a) => [a] -> [a]
alternate x = intermix x (negate <$> x)
         
type instance Negated Natural = Integer
type instance Negated Word = Int
type instance Negated Word8 = Int8
type instance Negated Word16 = Int16
type instance Negated Word32 = Int32
type instance Negated Word64 = Int64


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
    negate x = sub 0 x
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
instance Negatable Natural where 
    negate x = sub 0 x
    {-# INLINE negate #-}
instance Negatable Word where 
    negate x = sub 0 x
    {-# INLINE negate #-}
instance Negatable Word8 where 
    negate x = sub 0 x
    {-# INLINE negate #-}
instance Negatable Word16 where 
    negate x = sub 0 x
    {-# INLINE negate #-}
instance Negatable Word32 where 
    negate x = sub 0 x
    {-# INLINE negate #-}
instance Negatable Word64 where 
    negate x = sub 0 x
    {-# INLINE negate #-}
    

-- Negatable tuples
-------------------------------------------------------------------------------
type Negatable2 a1 a2 = (Negatable a1, Negatable a2)
type Negatable3 a1 a2 a3 = (Negatable2 a1 a2, Negatable a3)
type Negatable4 a1 a2 a3 a4 = (Negatable3 a1 a2 a3, Negatable a4)
type Negatable5 a1 a2 a3 a4 a5 = (Negatable4 a1 a2 a3 a4, Negatable a5)

instance Negatable a => Negatable (UniTuple1 a) where
    negate (UniTuple1 x) = UniTuple1 (negate x)

instance Negatable2 a1 a2 => Negatable (Tuple2 a1 a2) where    
    negate (a1,a2) = (negate a1, negate a2)

instance Negatable3 a1 a2 a3 => Negatable (Tuple3 a1 a2 a3) where
    negate (a1,a2,a3) = (negate a1, negate a2, negate a3)

instance Negatable4 a1 a2 a3 a4 => Negatable (Tuple4 a1 a2 a3 a4) where
    negate (a1,a2,a3,a4) = (negate a1, negate a2, negate a3, negate a4)

instance Negatable5 a1 a2 a3 a4 a5  => Negatable (Tuple5 a1 a2 a3 a4 a5)  where
    negate (a1,a2,a3,a4,a5) = (negate a1, negate a2, negate a3, negate a4, negate a5)                    