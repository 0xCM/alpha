module Alpha.Canonical.Algebra.Negatable
(    
    Negatable(..),
    Negated(..), 
    Binegatable(..),

) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations.Tuples
import Alpha.Canonical.Algebra.Divisive


-- | Characterizes types whose values are closed under 
-- additive negation
class Negatable a where
    -- | Negates the operand    
    negate::a -> a

-- Defines a family of types that represent the result of applying a
-- (potentially) heterogeneous negation operation
type family Negated a

-- / Characterizes types for which unary negation is defined
class Binegatable a where
    -- | Negates the operand    
    binegate::a -> Negated a


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
                    
type instance Negated Natural = Integer
type instance Negated Word = Int
type instance Negated Word8 = Int8
type instance Negated Word16 = Int16
type instance Negated Word32 = Int32
type instance Negated Word64 = Int64


