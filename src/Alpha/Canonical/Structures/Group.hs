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
    Inverter(..),
    Identity(..), 
    abs,
    group,
    
) where
import Alpha.Canonical.Structures.Common as X
import Alpha.Canonical.Structures.Structure as X
import Alpha.Canonical.Structures.Magma as X
import Alpha.Canonical.Structures.Semiring as X

-- | A multiplicative group with unspecified commutativity
newtype ProductGroup a = ProductGroup a
    deriving (Eq,Generic,Data,Typeable,Show)
instance Newtype (ProductGroup a)    

type instance Individual (ProductGroup a) = a    

newtype GroupMorphism a b = GroupMorphism (a -> b)    

class Identity a where
    identity::Individual a

class Identity a => Inverter a where
    invert::Individual a -> Individual a

class Composer a where
    compose::Individual a -> Individual a -> Individual a


class Group a where
    inverse::Individual a -> Individual a

    unit::Individual a

    combine::Individual a -> Individual a -> Individual a

-- | Represents the category whose objects are groups and morphisms are homomorphisms
-- See: https://www.ncatlab.org/nlab/show/Grp
class Category g => Grp g where


data instance ObjSet (Grp g) = GrpObj
data instance EndSet (Grp g) (Group a) (Group b) = GrpHom a b


group::(Group g, Newtype g) => O g -> g
group element = wrap element

-- | Constructs a commutator for a binary operator
-- See https://en.wikipedia.org/wiki/Commutator
commutator::forall g a . (Inverter a) => O2 (Individual a) -> O2 (Individual a)
commutator o  =  \x y ->  o (o (i x) (i y)) (o x y) where
    i = invert @a
 
abs::(Nullary a, TotalOrd a, Negatable a) => a -> a
abs a = ifelse (a >= zero) a (negate a)

instance Unital a => Identity (ProductGroup a) where
    identity = one

instance (Unital a, Reciprocative a) => Inverter (ProductGroup a) where
    invert = reciprocal

instance (Multiplicative a) => Composer (ProductGroup a) where
    compose = (*)

instance (a ~ Individual (ProductGroup a),  ProductMonoid a, Unital a, Reciprocative a) => Group (ProductGroup a) where
    inverse = reciprocal
    unit = one
    combine = (*)
    
-- | An additive group, always commutative
class (AdditiveMonoid a, Negatable a) => AbelianGroup a where  
    
class (AbelianGroup a, Finite a) => FiniteAbelianGroup a where


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
instance (Eq a, Negatable a, Additive a,Nullary a) => AbelianGroup (Complex a)
instance AbelianGroup Rational

type AG2 a1 a2 = (AbelianGroup a1, AbelianGroup a2)
type AG3 a1 a2 a3 = (AG2 a1 a2, AbelianGroup a3)
type AG4 a1 a2 a3 a4 = (AG3 a1 a2 a3, AbelianGroup a4)
type AG5 a1 a2 a3 a4 a5 = (AG4 a1 a2 a3 a4, AbelianGroup a5)

instance AG2 a1 a2 => AbelianGroup (Tuple2 a1 a2)
instance AG3 a1 a2 a3 => AbelianGroup (Tuple3 a1 a2 a3)
instance AG4 a1 a2 a3 a4 => AbelianGroup (Tuple4 a1 a2 a3 a4)
instance AG5 a1 a2 a3 a4 a5 => AbelianGroup (Tuple5 a1 a2 a3 a4 a5)