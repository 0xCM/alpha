-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
module Alpha.Data.VecN
(
    VecN(..),
    vecN
)
where
import Alpha.Canonical

import qualified Data.Vector as Vector
import qualified Data.List as List


newtype VecN n a = VecN (Vector a)
    deriving (Eq,Ord,Generic,Functor,Data,Typeable,Applicative,Foldable,Traversable,Monad,IsList)
instance Newtype (VecN n a)    

type instance Individual (VecN n a) = a

type VecNPair n a = (VecN n a, VecN n a)

vecN::forall n a. KnownNat n => [a] -> VecN n a
vecN x = VecN (Vector.fromList x)

instance KnownNat n => Vectored (VecN n a) a where
    vector (VecN v) = v

instance forall n a. (KnownNat n, Eq a) => Componentized (VecN n a) where
    type Component (VecN n a) = a
    components (VecN v) = toList v

instance forall n a. (Eq a,KnownNat n) => Componentized (VecNPair n a) where
    type Component (VecNPair n a) = (a,a)
    components ((VecN v1),(VecN v2)) = Vector.zipWith (\x y -> (x,y)) v1 v2 |> toList
    
instance forall n a. (KnownNat n, Formattable a, Eq a) => Formattable (VecN n a) where
    format v = v |> components |> tuplestring

instance forall n a. (KnownNat n, Formattable a, Eq a) => Show (VecN n a) where    
    show = string . format

combine::forall n a. (Eq a,KnownNat n) => O2 a -> VecNPair n a -> VecN n a    
combine f v = vecN @n ((\(x,y) -> f x y) <$> components v)

instance forall n a. (KnownNat n, Additive a, Eq a) => Additive (VecN n a) where
    v1 + v2 = combine (+) (v1,v2)

instance forall n a. (KnownNat n, Negatable a, Eq a) => Negatable (VecN n a) where
    negate v = negate <$> v 
    
instance forall n a. (KnownNat n, Subtractive a, Eq a) => Subtractive (VecN n a) where
    v1 - v2 = combine (-) (v1,v2)

instance forall n a. (KnownNat n, Multiplicative a, Eq a) => Multiplicative (VecN n a) where
    v1 * v2 = combine (*) (v1,v2)
        
instance forall n a. (KnownNat n, Nullary a, Eq a) => Nullary (VecN n a) where
    zero = clone (nat @n) zero |> vecN @n

instance forall n a. (KnownNat n, Unital a, Eq a) => Unital (VecN n a) where
    one =  clone (nat @n) one |> vecN @n 

    
-- instance forall k n. (KnownNat n, VectorSpace k) => InnerProductSpace (VecN n k) where
--     dot (VecN v1) (VecN v2) = result where
--         result = f <$> nats @n |> reduce zero (+)

--         f::Int -> k
--         f i = (v1 ! i) * (v2 ! i)         