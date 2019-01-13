-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

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
    vecN

) where
import Alpha.Canonical.Algebra as X
import Alpha.Canonical.Structures.Common as X
import Alpha.Canonical.Structures.Field as X
import Alpha.Canonical.Structures.Module as X
import Alpha.Canonical.Structures.Semiring as X


import qualified Data.Vector as Vector
import qualified Data.List as List

-- | Represents a pair of vectors
newtype VecPair a = VecPair (Vector a, Vector a)
    deriving (Eq,Ord,Generic,Data,Typeable)
instance Newtype (VecPair a)    

-- Represents a vector with type-level dimension
newtype VecN n a = VecN (Vector a)
    deriving (Eq,Ord,Generic,Data,Typeable,Functor,
        Applicative,Foldable,Traversable,Monad,IsList,
        Additive,Subtractive,Negatable,Multiplicative,
        Componentized,Indexable,Length,Queryable)
    deriving Formattable via (Vector a)
instance Newtype (VecN n a)    


-- | Represents a pair of n-vectors
newtype VecNPair n a = VecNPair (VecPair a) 
    deriving (Eq,Ord,Generic,Data,Typeable,Componentized)
instance Newtype (VecNPair n a)    

type instance Individual (Vector a) = a
type instance Individual (VecN n a) = a
type instance Individual (VecNPair n a) = (a,a)
type instance Individual (VecPair a) = (a,a)

class (Field k, LeftModule k v, Dimensional v) => VectorSpace k v where

class (KnownNat n, VectorSpace k v) => VectorSpaceN n k v where
    dim::(Integral i) => i
    dim = natg @n

class (Componentized v , Semiring (Individual v)) => InnerProduct v where
    dot::v -> v -> Individual v
    dot v1 v2 = reduce zero (+) [x * y | (x,y) <- List.zip (components v1) (components v2)]
        

    (.*.)::v -> v -> Individual v
    (.*.) = dot

class (VectorSpace k v, InnerProduct v) => InnerProductSpace k v where

data instance Space (VectorSpace k v) = VectorSpace v
data instance Space (InnerProductSpace k v) = InnerProductSpace v
    

vecN::forall n a. KnownNat n => [a] -> VecN n a
vecN x = VecN (Vector.fromList x)

applyVecN::forall n a. KnownNat n => O2 a -> VecNPair n a -> VecN n a    
applyVecN f v = vecN @n ((\(x,y) -> f x y) <$> components v)

applyVec::O2 a -> VecPair a -> Vector a    
applyVec f v = fromList ((\(x,y) -> f x y) <$> components v)

vecpair::Vector a -> Vector a -> VecPair a
vecpair v1 v2 = VecPair (v1,v2)

vecpairN::forall n a. KnownNat n => VecN n a -> VecN n a -> VecNPair n a
vecpairN (VecN v1) (VecN v2) = VecNPair (vecpair v1 v2)


-- Structure instances
-------------------------------------------------------------------------------
instance Structure 2 VectorSpace
instance StructureN 2 VectorSpaceN
instance Structure 2 InnerProductSpace

-- Vector instances
-------------------------------------------------------------------------------
instance Vectored (Vector a) a where
    vector  = id

instance Componentized (Vector a) where
    components = toList

instance Queryable (Vector a) where
    filter pred source =  toList source |> filter pred

instance Length (Vector a) where
    length = fromIntegral . Vector.length
    
instance Componentized (VecPair a) where
    components (VecPair (v1,v2)) = Vector.zipWith (\x y -> (x,y)) v1 v2 |> toList
    
instance (Formattable a) => Formattable (Vector a) where
    format v = v |> components |> tuplestring

instance (Additive a) => Additive (Vector a) where
    v1 + v2 = applyVec (+) (VecPair (v1,v2))
    
instance Negatable a => Negatable (Vector a) where
    negate v = negate <$> v 
   
instance Subtractive a => Subtractive (Vector a) where
    v1 - v2 = applyVec (-) (vecpair v1 v2)

instance Multiplicative a => Multiplicative (Vector a) where
    v1 * v2 = applyVec (*) (vecpair v1 v2)  where
                
instance Semiring a => InnerProduct (Vector a)


-- VecN instances    
-------------------------------------------------------------------------------
instance KnownNat n => Vectored (VecN n a) a where
    vector (VecN v) = v

instance forall n a. (KnownNat n, Formattable a) => Show (VecN n a) where    
    show = string . format
        
instance forall n a. (KnownNat n, Unital a) => Unital (VecN n a) where
    one =  clone (nat @n) one |> vecN @n 

instance forall n a. (KnownNat n, Nullary a) => Nullary (VecN n a) where
    zero = clone (nat @n) zero |> vecN @n

instance forall n a. (KnownNat n, AbelianGroup a) => AbelianGroup (VecN n a)
        
instance forall n k a. (KnownNat n, LeftAction k a) => LeftAction k (VecN n a) where
    k *. v = (\c -> k *. c) <$> components v |> vecN

instance forall n a. (KnownNat n, Semiring a) => InnerProduct (VecN n a)
    
instance forall n k a. (KnownNat n, Ring k, LeftModule k a) => LeftModule k (VecN n a)

instance forall n k a. (KnownNat n, Field k, VectorSpace k a) => VectorSpace k (VecN n a)

instance forall n a. KnownNat n => Dimensional (VecN n a) where
    type Dimension (VecN n a) = Natural
    dimension _ = natg @n

        