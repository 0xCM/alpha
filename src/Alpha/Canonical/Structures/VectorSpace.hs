-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Structures.VectorSpace
(
    module X,
    VectorSpace(..),    
    VectorSpaceN(..),
    InnerProduct(..),
    InnerProductSpace(..),
    VecN(..),
    VecPair(..),
    VecNPair(..),
    VecNBasis(..),
    NVectored(..),
    vecN

) where
import Alpha.Canonical.Algebra as X
import Alpha.Canonical.Structures.Common as X
import Alpha.Canonical.Structures.Field as X
import Alpha.Canonical.Structures.Module as X
import Alpha.Canonical.Structures.Semiring as X

import qualified Data.Vector.Storable as Storable
import qualified Data.Vector as Vector
import qualified Data.List as List

-- | Represents a vector with type-level dimension
newtype VecN n a = VecN (Vector a)
    deriving (Eq,Ord,Generic,Data,Typeable,Functor,
        Applicative,Foldable,Traversable,Monad,IsList,
        Additive,Subtractive,Negatable,Multiplicative,
        Discrete,Indexable,Length,Queryable,Listed)
    deriving Formattable via (Vector a)
instance Newtype (VecN n a)    
type instance Individual (VecN n a) = a

-- | Represents a pair of n-vectors
newtype VecNPair n a = VecNPair (VecPair a) 
    deriving (Eq,Ord,Generic,Data,Typeable,Discrete)
instance Newtype (VecNPair n a)    
type instance Individual (VecNPair n a) = (a,a)

-- | Represents a basis in an n-dimensional vector space
newtype VecNBasis n a = VecNBasis [VecN n a]
    deriving (Eq,Ord,Generic,Data,Typeable,Functor, Foldable,Traversable)
instance Newtype (VecNBasis n a)    

class (Field k, LeftModule k v, Dimensional v) => VectorSpace k v where

class (KnownNat n, VectorSpace k v) => VectorSpaceN n k v where
    dim::(Integral i) => i
    dim = natg @n

class (Discrete v , Semiring (Individual v)) => InnerProduct v where
    dot::v -> v -> Individual v
    dot v1 v2 = reduce zero (+) [x * y | (x,y) <- pairzip (individuals v1) (individuals v2)]        

    (.*.)::v -> v -> Individual v
    (.*.) = dot
    {-# INLINE (.*.) #-}

class (VectorSpace k v, InnerProduct v) => InnerProductSpace k v where

--data instance Space (VectorSpace k a) (VecN n a) = VecNSpace

type Euclidean n k v = (KnownNat n, v ~ VecN n k, AbelianGroup v,  Ring k, LeftAction k v)


class KnownNat n => NVectored n s where
    nvector::s -> VecN n (Individual s)


vecN::forall n a. KnownNat n => [a] -> VecN n a
vecN x = VecN (Vector.fromList x)

applyVecN::forall n a. KnownNat n => O2 a -> VecNPair n a -> VecN n a    
applyVecN f v = vecN @n ((\(x,y) -> f x y) <$> individuals v)

vecpairN::forall n a. KnownNat n => VecN n a -> VecN n a -> VecNPair n a
vecpairN (VecN v1) (VecN v2) = VecNPair (vecpair v1 v2)
                
instance KnownNat n => NVectored n [a] where
    nvector = vecN

-- *Vector membership
-------------------------------------------------------------------------------
    
instance Semiring a => InnerProduct (Vector a)
    
-- *VecN membership
-------------------------------------------------------------------------------
instance KnownNat n => Vectored (VecN n a) where
    vector (VecN v) = v

instance forall n a. (KnownNat n, Formattable a) => Show (VecN n a) where    
    show = string . format
        
instance forall n a. (KnownNat n, Unital a) => Unital (VecN n a) where
    one =  clone (nat @n) one |> vecN @n 

instance forall n a. (KnownNat n, Nullary a) => Nullary (VecN n a) where
    zero = clone (nat @n) zero |> vecN @n

instance forall n a. (KnownNat n, AbelianGroup a) => AbelianGroup (VecN n a)
        
instance forall n k a. (KnownNat n, LeftAction k a) => LeftAction k (VecN n a) where
    k *. v = (\c -> k *. c) <$> individuals v |> vecN

instance forall n a. (KnownNat n, Semiring a) => InnerProduct (VecN n a)

instance forall n k a. (KnownNat n, Ring k, LeftModule k a) => LeftModule k (VecN n a)

instance forall n k a. (KnownNat n, Field k, VectorSpace k a) => VectorSpace k (VecN n a)

instance forall n k a. (KnownNat n, Semiring a, VectorSpace k a) => InnerProductSpace k (VecN n a)

instance forall n a. KnownNat n => Dimensional (VecN n a) where
    type Dimension (VecN n a) = Natural
    dimension _ = natg @n