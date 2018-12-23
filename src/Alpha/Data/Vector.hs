-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
module Alpha.Data.Vector
(
    VecN(..),
    ScalarProduct(..),
    vecN
)
where
import Alpha.Canonical
import Alpha.Data.NatK
import qualified Data.Vector as Vector
import qualified Data.List as List

class (Vectored v) => ScalarProduct v where

    type Dotted v
    dot::v -> v -> Dotted v

    (.*.)::v -> v -> Dotted v
    (.*.) = dot

newtype VecN n a = VecN (Vector a)
    deriving (Eq,Ord,Generic)
instance Newtype (VecN n a)    

type instance Element (VecN n a) = a

type VecNPair n a = (VecN n a, VecN n a)

vecN::forall n a. KnownNat n => [a] -> VecN n a
vecN x = VecN (Vector.fromList x)

instance KnownNat n => Vectored (VecN n a) where
    vector (VecN v) = v

instance forall n a. (KnownNat n, Eq a) => Componentized (VecN n a) where
    type Component (VecN n a) = a
    components (VecN v) = members v

instance forall n a. (Eq a,KnownNat n) => Componentized (VecNPair n a) where
    type Component (VecNPair n a) = (a,a)
    components ((VecN v1),(VecN v2)) = Vector.zipWith (\x y -> (x,y)) v1 v2 |> members
    
instance forall n a. (KnownNat n, Formattable a, Eq a) => Formattable (VecN n a) where
    format v = v |> components |> tuplestring

instance forall n a. (KnownNat n, Formattable a, Eq a) => Show (VecN n a) where    
    show = string . format

instance forall k n. (KnownNat n, Ring k) => ScalarProduct (VecN n k) where
    type Dotted (VecN n k) = k
    dot (VecN v1) (VecN v2) = result where
        result = f <$> nats @n |> reduce zero (+)

        f::Int -> k
        f i = (v1 ! i) * (v2 ! i)         