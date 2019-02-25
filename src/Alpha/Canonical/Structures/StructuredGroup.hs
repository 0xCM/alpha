-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Structures.StructuredGroup
(
    module X,
    ProductGroup(..),
    GroupMorphism(..),
    InversionStructure(..),
    IdentityStructure(..),
    
) where
import Alpha.Canonical.Structures.Common as X
import Alpha.Canonical.Structures.Magma as X
import Alpha.Canonical.Structures.Semiring as X

-- | A multiplicative group with unspecified commutativity
newtype ProductGroup a = ProductGroup a
    deriving (Eq,Generic,Data,Typeable,Show)
instance Newtype (ProductGroup a)    
type instance Individual (ProductGroup a) = a    

newtype GroupMorphism a b = GroupMorphism (a -> b)    

class IdentityStructure a where
    sidentity::Individual a

class IdentityStructure a => InversionStructure a where
    sinvert::Individual a -> Individual a

class CompositionStructure a where
    scompose::Individual a -> Individual a -> Individual a

class GroupStructure a where
    sinverse::Individual a -> Individual a

    sunit::Individual a

    scombine::Individual a -> Individual a -> Individual a

-- | Represents the category whose objects are groups and morphisms are homomorphisms
-- See: https://www.ncatlab.org/nlab/show/Grp
class Category g => Grp g where
data instance ObjSet (Grp g) = GrpObj
data instance EndSet (Grp g) (GroupStructure a) (GroupStructure b) = GrpHom a b

-- | Constructs a commutator for a binary operator
-- See https://en.wikipedia.org/wiki/Commutator
commutator::forall g a . (InversionStructure a) => O2 (Individual a) -> O2 (Individual a)
commutator o  =  \x y ->  o (o (i x) (i y)) (o x y) where
    i = sinvert @a

instance Unital a => IdentityStructure (ProductGroup a) where
    sidentity = one

instance (Unital a, Invertible a) => InversionStructure (ProductGroup a) where
    sinvert = invert

instance (Multiplicative a) => CompositionStructure (ProductGroup a) where
    scompose = (*)

instance (a ~ Individual (ProductGroup a),  ProductMonoid a, Unital a, Invertible a) => GroupStructure (ProductGroup a) where
    sinverse = invert
    sunit = one
    scombine = (*)
    