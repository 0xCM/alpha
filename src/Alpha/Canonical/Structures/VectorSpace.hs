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
    InnerProductSpace(..),
    VecN(..),
    VecNPair(..),
    vecN

) where
import Alpha.Canonical.Algebra as X
import Alpha.Canonical.Structures.Field as X
import Alpha.Canonical.Structures.Module as X
import qualified Data.Vector as Vector
import qualified Data.List as List

newtype VecN n a = VecN (Vector a)
    deriving (Eq,Ord,Generic,Functor,Data,Typeable,Applicative,Foldable,Traversable,Monad,IsList)
instance Newtype (VecN n a)    

type instance Individual (VecN n a) = a
type instance Individual (VecNPair n a) = (a,a)

newtype VecNPair n a = VecNPair (VecN n a, VecN n a)
    deriving (Eq,Ord,Generic,Data,Typeable)
instance Newtype (VecNPair n a)    

class (Field k, LeftModule k v) => VectorSpace k v where

class (KnownNat n, VectorSpace k v) => VectorSpaceN n k v where
    dim::(Integral i) => i
    dim = natg @n

class (VectorSpace k v) => InnerProductSpace k v where
    dot::v -> v -> k

    (.*.)::v -> v -> k
    (.*.) = dot

vecN::forall n a. KnownNat n => [a] -> VecN n a
vecN x = VecN (Vector.fromList x)

vecapply::forall n a. KnownNat n => O2 a -> VecNPair n a -> VecN n a    
vecapply f v = vecN @n ((\(x,y) -> f x y) <$> components v)

instance Structure 2 VectorSpace
instance StructureN 2 VectorSpaceN
instance Structure 2 InnerProductSpace

instance KnownNat n => Vectored (VecN n a) a where
    vector (VecN v) = v

instance forall n a. KnownNat n => Componentized (VecN n a) where
    components (VecN v) = toList v

instance forall n a. KnownNat n => Componentized (VecNPair n a) where
    components (VecNPair ((VecN v1),(VecN v2))) = Vector.zipWith (\x y -> (x,y)) v1 v2 |> toList
    
instance forall n a. (KnownNat n, Formattable a) => Formattable (VecN n a) where
    format v = v |> components |> tuplestring

instance forall n a. (KnownNat n, Formattable a) => Show (VecN n a) where    
    show = string . format

instance forall n a. (KnownNat n, Additive a) => Additive (VecN n a) where
    v1 + v2 = vecapply (+) (VecNPair (v1,v2))

instance forall n a. (KnownNat n, Negatable a) => Negatable (VecN n a) where
    negate v = negate <$> v 
    
instance forall n a. (KnownNat n, Subtractive a) => Subtractive (VecN n a) where
    v1 - v2 = vecapply (-) (VecNPair (v1,v2))

instance forall n a. (KnownNat n, Multiplicative a) => Multiplicative (VecN n a) where
    v1 * v2 = vecapply (*) (VecNPair (v1,v2))  where
        

instance forall n a. (KnownNat n, Unital a) => Unital (VecN n a) where
    one =  clone (nat @n) one |> vecN @n 
            
instance forall n k. (KnownNat n, Multiplicative k) => LeftAction k (VecN n k) where
    k *. v = (\c -> c * k) <$> components v |> vecN

instance forall n a. (KnownNat n, Nullary a) => Nullary (VecN n a) where
    zero = clone (nat @n) zero |> vecN @n


instance forall n a. (KnownNat n, AbelianGroup a) => AbelianGroup (VecN n a)
    
    