-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Structures.Group
(
    module X,
    Group(..),
    AbelianGroup(..),
    FiniteAbelianGroup(..),
    Punctured(..),
    
) where
import Alpha.Canonical.Structures.Common as X
import Alpha.Canonical.Structures.Structure as X
import Alpha.Canonical.Structures.Magma as X
import Alpha.Canonical.Structures.Semiring as X

class (Semigroup a, Unital a, Multiplicative a, Invertible a) => Group a where
    -- | Specifies the unit element of the group
    unit::a
    unit = one
    {-# INLINE unit #-}

    -- | The binary group operation
    compose::a -> a -> a
    compose = (*)
    {-# INLINE compose #-}    

class (KnownNat n, Group a, FinitelyCountable a) => FiniteGroup n a where

-- | An additive group, always commutative
class (AdditiveMonoid a, Negatable a) => AbelianGroup a where  
    
-- | A group with a basis
-- See https://en.wikipedia.org/wiki/Free_abelian_group
class AbelianGroup a => FreeAbelianGroup a where
    basis::a -> [a]    

class (AbelianGroup a, FinitelyCountable a) => FiniteAbelianGroup a where
    
instance Integral a => Semigroup (Ratio a)  where
    (<>) = (*)
    {-# INLINE (<>) #-}

instance Semigroup Float  where
    (<>) = (*)
    {-# INLINE (<>) #-}

instance Semigroup NonzeroDouble where
    a <> b =  wrap ( (unwrap a) * (unwrap b))
    {-# INLINE (<>) #-}
        
-- | Note: for this to really be a group, the 0 element must be deleted
instance Integral a => Group (Ratio a)    
instance Group Float where 
    
instance AbelianGroup Integer where 
instance AbelianGroup Int where 
instance AbelianGroup Int8 where 
instance AbelianGroup Int16 where 
instance AbelianGroup Int32 where 
instance AbelianGroup Int64 where     
instance AbelianGroup Natural where 
instance AbelianGroup Word where 
instance AbelianGroup Word8 where 
instance AbelianGroup Word16 where 
instance AbelianGroup Word32 where 
instance AbelianGroup Word64 where     
instance (Integral a) => AbelianGroup (Ratio a) where 
instance Integral a => AbelianGroup (RationalNumber a)
instance AbelianGroup Float where 
instance AbelianGroup Double where 
instance AbelianGroup CFloat where 
instance AbelianGroup CDouble where                                 
instance (Eq a, Negatable a, Additive a,Nullary a) => AbelianGroup (ComplexNumber a)


type AG2 a1 a2 = (AbelianGroup a1, AbelianGroup a2)
type AG3 a1 a2 a3 = (AG2 a1 a2, AbelianGroup a3)
type AG4 a1 a2 a3 a4 = (AG3 a1 a2 a3, AbelianGroup a4)
type AG5 a1 a2 a3 a4 a5 = (AG4 a1 a2 a3 a4, AbelianGroup a5)

instance AG2 a1 a2 => AbelianGroup (Tuple2 a1 a2)
instance AG3 a1 a2 a3 => AbelianGroup (Tuple3 a1 a2 a3)
instance AG4 a1 a2 a3 a4 => AbelianGroup (Tuple4 a1 a2 a3 a4)
instance AG5 a1 a2 a3 a4 a5 => AbelianGroup (Tuple5 a1 a2 a3 a4 a5)