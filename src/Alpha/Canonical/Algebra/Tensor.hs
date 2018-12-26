-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}

module Alpha.Canonical.Algebra.Tensor
(
    IndexComponent(..),
    ComponentSpec(..),
    lowerIndex,
    upperIndex,
    
)
where
import Alpha.Canonical.Relations
import qualified Alpha.Canonical.Common.Asci as Asci

    
type Indicial s n = (KnownSymbol s, KnownNat n)

-- | Represents a constituent in a tensor index scheme and carries
-- the index label and position at the type level while its
-- value indicates whether the index is upper (or contravariant) or 
-- lower (or covariant)
newtype IndexComponent s n = IndexComponent Variance
    deriving (Eq, Ord)

-- | Describes a 'TensorComponent' completely at the value-level    
newtype ComponentSpec = ComponentSpec (Variance, Text, Word)
    deriving (Eq, Ord)


-- | Defines a collection of index complexes as a unit that consists
-- of an ordered pair of contravariant and covariant components    
newtype IndexComplex = IndexComplex ([ComponentSpec],[ComponentSpec])
    deriving (Eq, Ord)

lowerIndex::Indicial s n => IndexComponent s n
lowerIndex = IndexComponent Covariant

upperIndex::Indicial s n => IndexComponent s n
upperIndex = IndexComponent Contravariant

instance forall s n. Indicial s n  => Specifiable (IndexComponent s n) where
    type Specified (IndexComponent s n) = ComponentSpec

    specify idx = ComponentSpec (variance idx, symtext @s, integral $ nat @n)

instance Indicial s n => Variant (IndexComponent s n) where    
    variance (IndexComponent v) = v
    
instance Variant (ComponentSpec) where
    variance (ComponentSpec (v,_,_)) = v

instance Show Variance where
    show = string . format

instance Formattable ComponentSpec where
    format (ComponentSpec (v,label,pos)) = format (v, label, pos)

instance Show ComponentSpec where
    show = string . format

instance Indicial s n =>  Formattable (IndexComponent s n) where
    format = format . specify

instance Indicial s n => Show (IndexComponent s n) where
    show = string . format

