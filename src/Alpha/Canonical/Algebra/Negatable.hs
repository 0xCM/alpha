module Alpha.Canonical.Algebra.Negatable
(    
    Negatable(..),

)
where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Relations.Tuples


-- / Characterizes types for which unary negation is be defined
class Negatable a where
    -- The range of negation
    type Negated a
    type Negated a = a

    -- | Negates the operand    
    negate::a -> Negated a

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

type NegatedTuple2 a1 a2 = Tuple2 (Negated a1) (Negated a2)
type NegatedTuple3 a1 a2 a3 = Tuple3 (Negated a1) (Negated a2) (Negated a3)
type NegatedTuple4 a1 a2 a3 a4 = Tuple4 (Negated a1) (Negated a2) (Negated a3) (Negated a4)   
type NegatedTuple5 a1 a2 a3 a4 a5 = Tuple5 (Negated a1) (Negated a2) (Negated a3) (Negated a4) (Negated a5)  

instance Negatable2 a1 a2 => Negatable (Tuple2 a1 a2) where
    type Negated (Tuple2 a1 a2) = NegatedTuple2 a1 a2
    negate (a1,a2) = (negate a1, negate a2)

instance Negatable3 a1 a2 a3 => Negatable (Tuple3 a1 a2 a3) where
    type Negated (Tuple3 a1 a2 a3) = NegatedTuple3 a1 a2 a3
    negate (a1,a2,a3) = (negate a1, negate a2, negate a3)

instance Negatable4 a1 a2 a3 a4 => Negatable (Tuple4 a1 a2 a3 a4) where
    type Negated (Tuple4 a1 a2 a3 a4) = NegatedTuple4 a1 a2 a3 a4
    negate (a1,a2,a3,a4) = (negate a1, negate a2, negate a3, negate a4)

instance Negatable5 a1 a2 a3 a4 a5  => Negatable (Tuple5 a1 a2 a3 a4 a5)  where
    type Negated (Tuple5 a1 a2 a3 a4 a5) = NegatedTuple5 a1 a2 a3 a4 a5
    negate (a1,a2,a3,a4,a5) = (negate a1, negate a2, negate a3, negate a4, negate a5)
                    