-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
module Alpha.Data.Matrix
(
    Matrix,
    matrix,
    matrix1
)
where

import Alpha.Canonical
import qualified Data.List as List

newtype Matrix m n a = Matrix (VecN m (VecN n a))
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable) 

type instance Individual (Matrix m n a) = a

matrix::forall m n a. (KnownNat m, KnownNat n) => [a] -> Matrix m n a
matrix rowmajor = Matrix $ vecN @m rows where
    (m,n) = (nat @m, nat @n)
    segments = segment n rowmajor
    rows = (vecN @n) <$> segments

instance forall m n a. (KnownNat m, KnownNat n) => Dimensional (Matrix m n a) where
    type Dimension (Matrix m n a) = (Int,Int)
    dimension matrix = (nat @m, nat @n)

instance Discrete (Matrix n m a) where
    individuals (Matrix vec) = list <$> list vec |> append

instance forall m n a. (KnownNat m, KnownNat n) => Transposable (Matrix m n a) where
    type Transposed (Matrix m n a) = Matrix n m a
    transpose m = matrix @n @m (cols m |> append)

instance forall m n a. (KnownNat m, KnownNat n) => Tabular (Matrix m n a) where
    rows (Matrix vec)  = list <$> list vec
    cols m =   transpose (rows m)

instance forall m n a. (KnownNat m, KnownNat n, Formattable a) => Formattable (Matrix m n a) where    
    format m =  format <$> (\row -> (spaced . format) <$> row ) <$> rows m |> weave EOL |> append

instance forall m n a. (KnownNat m, KnownNat n, Formattable a) => Show (Matrix m n a) where    
    show = string . format
    
matrix1 = matrix @3 @2 [1,2,3,4,5,6]

