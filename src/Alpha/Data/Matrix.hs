-----------------------------------------------------------------------------
-- | 0 1
-- Copyright   :  (c) Chris Moore, 2018 + Contributors per license
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
module Alpha.Data.Matrix
(
    Matrix,
    matrix,
)
where

import Alpha.Canonical
import qualified Data.List as List

newtype Matrix m n a = Matrix (VecN m (VecN n a))
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable) 

type instance Individual (Matrix m n a) = a

instance forall m n a. (KnownNat m, KnownNat n) => Dimensional (Matrix m n a) where
    type Dimension (Matrix m n a) = (Int,Int)
    dimension matrix = (nat @m, nat @n)

matrix::forall m n a. (KnownNat m, KnownNat n) => [a] -> Matrix m n a
matrix rowmajor = Matrix $ vecN @m rows where
    (m,n) = (nat @m, nat @n)
    segments = segment n rowmajor
    rows = (vecN @n) <$> segments

instance Discrete (Matrix n m a) where
    individuals (Matrix vec) = list <$> list vec |> append
        
instance forall m n a. (KnownNat m, KnownNat n) => Tabular (Matrix m n a) where
    rows (Matrix vec)  = list <$> list vec
    cols m =   transpose (rows m)

instance forall m n a. (KnownNat m, KnownNat n, Formattable a) => Formattable (Matrix m n a) where    
    format m =  format <$> (\row -> (spaced . format) <$> row ) <$> rows m |> weave EOL |> append

instance forall m n a. (KnownNat m, KnownNat n, Formattable a) => Show (Matrix m n a) where    
    show = string . format
    
