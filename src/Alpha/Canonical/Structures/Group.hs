-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Structures.Group
(
    module X,
    ProductGroup(..),
    AbelianGroup(..),
    FiniteAbelianGroup(..),
    GroupMorphism(..),
    commutator,
    Invertible(..),
    Inverter(..), 
    Identity(..), 
    abs,
    
) where
import Alpha.Canonical.Algebra
import Alpha.Canonical.Structures.Structure as X
import Alpha.Canonical.Structures.Monoid as X
import Alpha.Canonical.Structures.Magma as X

class Identity a where
    identity::a

class Invertible a where
    invert::a -> a

class UnaryOperator a => Inverter a where
    inverter::O1 a
    inverter = o1
    
newtype GroupMorphism a b = GroupMorphism (a -> b)    

instance Morphic GroupMorphism AbelianGroup a b where
    morphism (GroupMorphism f) = f

class (Magma a, Inverter a, Identity a) => Group a where
    inverse::a -> a
    inverse = inverter

    unit::a
    unit = identity

    combine::a -> a -> a
    combine = composite
    

-- | A multiplicative group, not necessarily commutive
class (ProductMonoid a, Reciprocative a) => ProductGroup a where
    
-- | An additive group, always commutative
class (SumMonoid a, Negatable a) => AbelianGroup a where  
    
class (AbelianGroup a, Finite a) => FiniteAbelianGroup a where


-- | Constructs a commutator for a binary operator
-- See https://en.wikipedia.org/wiki/Commutator
commutator::forall g a . (Inverter a) => O2 a -> O2 a
commutator o  =  \x y ->  o (o (i x) (i y)) (o x y) where
    i = inverter @a
 
abs::(Nullary a, TotalOrd a, Negatable a) => a -> a
abs a = ifelse (a >= zero) a (negate a)

instance Structure 1 FiniteAbelianGroup
instance Structure 1 Group
instance Structure 1 ProductGroup
instance Structure 1 AbelianGroup


instance Integral a => ProductGroup (Ratio a) where
instance ProductGroup Float where 
instance ProductGroup Double where 
instance ProductGroup CFloat where 
instance ProductGroup CDouble where 

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
instance AbelianGroup Float where 
instance AbelianGroup Double where 
instance AbelianGroup CFloat where 
instance AbelianGroup CDouble where                         
        

type AG2 a1 a2 = (AbelianGroup a1, AbelianGroup a2)
type AG3 a1 a2 a3 = (AG2 a1 a2, AbelianGroup a3)
type AG4 a1 a2 a3 a4 = (AG3 a1 a2 a3, AbelianGroup a4)
type AG5 a1 a2 a3 a4 a5 = (AG4 a1 a2 a3 a4, AbelianGroup a5)

instance AG2 a1 a2 => AbelianGroup (Tuple2 a1 a2)
instance AG3 a1 a2 a3 => AbelianGroup (Tuple3 a1 a2 a3)
instance AG4 a1 a2 a3 a4 => AbelianGroup (Tuple4 a1 a2 a3 a4)
instance AG5 a1 a2 a3 a4 a5 => AbelianGroup (Tuple5 a1 a2 a3 a4 a5)
