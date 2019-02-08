-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Linear.Matrix
(
    Matrix(..),
    RowSlice(..),
    ColSlice(..),
    MatrixCell(..),
    cells,
    matrix,
    rowslice,
    rowdata,
    colslice,
    coldata,
    indices,
    matrange,
)
where

-- TODO Define a (left|right) action using a permutation on a matrix to change (rows|columns)
import Alpha.Canonical
import qualified Data.List as List

-- | Represents a matrix
newtype Matrix m n a = Matrix (VecN m (VecN n a))
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable)     
type instance Individual (Matrix m n a) = a
type instance Multiplied (Matrix m n a) (Matrix n p a) = Matrix m p a
    
-- | Represents a 1 x n matrix
type RowSlice n a = Matrix 1 n a

-- | Represents a n x 1 matrix
type ColSlice m a = Matrix m 1 a

-- | Represents a cell in a matrix
type MatrixCell v = Cell (Int,Int) v
        
-- Constructs a matrix from a list where the elements are in row-major order
matrix::forall m n a. (KnownNatPair m n) => [a] -> Matrix m n a
matrix rowmajor = Matrix $ vecN @m rows where
    (m,n) = (nat @m, nat @n)
    segments = segment n rowmajor
    rows = (vecN @n) <$> segments

-- | Extracts matrix data using row-major convention
rowdata::forall m n a .(KnownNatPair m n) => Matrix m n a -> [[a]]
rowdata (Matrix vec)  = list <$> list vec

-- | Extracts matrix data using column-major convention
coldata::forall m n a .(KnownNatPair m n) => Matrix m n a -> [[a]]
coldata =  transpose . rowdata 

-- | Extracts a 1 x n matrix from a m x n matrix
rowslice::forall m n a.(KnownNatPair m n) => Int -> Matrix m n a -> RowSlice n a
rowslice i (Matrix m) = m !! i |> list |> matrix

-- | Extracts a m x 1 matrix from a m x n matrix
colslice::forall m n a.(KnownNatPair m n) => Int -> Matrix m n a -> ColSlice m a
colslice i m = transpose m |> rowslice i |> transpose 

-- | Calculates the index values for a (dense) matrix
indices::forall m n. (KnownNatPair m n) => [(Int,Int)]
indices = [(r,c) | r <- natrange @m, c <- natrange @n] where
    
-- Creates a test matrix of integer values
matrange::forall m n. (KnownNatPair m n) => Matrix m n Int
matrange = matrix [0..max] where
    max =  (natmul @m @n - 1) 

cells::forall m n a. (KnownNatPair m n) => Matrix m n a -> [Cell (Int,Int) a]
cells m = (\ix -> Cell(ix , m !!ix)) <$> indices @m @n

-- *Matrix class membership 
-------------------------------------------------------------------------------
instance Indexable (Matrix m n a) where
    type Indexer (Matrix m n a) = (Int,Int)
    idx (Matrix m) (i,j) = (m !! i) !! j

instance forall m n p s a. (KnownNatPair m n, KnownNatPair m p, KnownNat (n + p), Eq a) => BiConcatenable (Matrix m n a) (Matrix m p a) where
    type BiConcatenated (Matrix m n a) (Matrix m p a)  = Matrix m (n + p) a
    biconcat a b = newRow <$> natrange @m |> append |> matrix @m @(n+p) where
        aRows = rowdata a
        bRows = rowdata b
        newRow i = (aRows !! i) ++ (bRows !! i)
                    
instance forall m n a. (KnownNat m, KnownNat n) => Dimensional (Matrix m n a) where
    type Dimension (Matrix m n a) = (Int,Int)
    dimension matrix = (nat @m, nat @n)

instance Discrete (Matrix n m a) where
    individuals (Matrix vec) = list <$> list vec |> append

instance forall m n a. (KnownNat m, KnownNat n) => Transposable (Matrix m n a) where
    type Transposed (Matrix m n a) = Matrix n m a
    transpose m = matrix @n @m (cols m |> append) where
        rows (Matrix vec)  = list <$> list vec
        cols m =   transpose (rows m)

instance forall m n a. (KnownNatPair m n, Nullary a) => Nullary (Matrix m n a) where
    zero = matrix @m @n (clone (natmul @m @n) zero) 
                    
instance forall n a. (KnownNat n, Unital a, Nullary a) => Unital (Matrix n n a) where
    one = matrix @n @n [ifelse (i == j) one zero | i <- natrange @n, j <- natrange @n]        

instance forall m n a. (KnownNatPair m n,  Additive a) => Additive (Matrix m n a) where
    add m1 m2 = matrix @m @n [ m1 !! (r,c) + m2 !! (r,c) | (r,c) <- indices @m @n]
    
instance forall m n a. (KnownNatPair m n,  Subtractive a) => Subtractive (Matrix m n a) where
    sub m1 m2 = matrix @m @n [ m1 !! (r,c) - m2 !! (r,c) | (r,c) <- indices @m @n]               

instance forall m n a. (KnownNatPair m n, Negatable a) => Negatable (Matrix m n a) where
    negate m = negate <$> m
        
instance forall m n a. (KnownNatPair m n, AbelianGroup a) => AbelianGroup (Matrix m n a) where

instance forall m n k a. (KnownNatPair m n, LeftAction k a) => LeftAction k (Matrix m n a) where
    k *. m = (\item -> k *. item) <$> m
    
instance forall m n a k. (KnownNatPair m n, RightAction a k) => RightAction (Matrix m n a) k where
    m .* k  = (\item -> item .* k) <$> m

instance forall m n a. (KnownNatPair m n, Complex a) => Complex (Matrix m n a) where
    conjugate m = conjugate <$> m
    re = undefined
    im = undefined

instance Formattable a => Formattable (Matrix m n a) where    
    format m =  format <$> (\row -> (pad . format) <$> row ) <$> rows m |> weave EOL |> append where
        rows (Matrix x)  = list <$> list x

instance Formattable a => Show (Matrix m n a) where    
    show = string . format
    
-- *RowSlice class membership 
-------------------------------------------------------------------------------

instance (KnownNat n) => NVectored n (RowSlice n a) where
    nvector m = individuals m |> vecN

instance forall n a. (KnownNat n, Multiplicative a)  => Multiplicative (RowSlice n a) where
    r1 * r2 = zip (\i1 i2 -> i1 * i2) (individuals r1) (individuals r2) |> matrix @1 @n

-- *ColSlice class membership 
-------------------------------------------------------------------------------
instance (KnownNat n) => NVectored n (ColSlice n a) where
    nvector m = individuals m |> vecN        

instance forall n a. (KnownNat n, Multiplicative a)  => Multiplicative (ColSlice n a) where
    r1 * r2 = zip (\i1 i2 -> i1 * i2) (individuals r1) (individuals r2) |> matrix @n @1

-- *MatrixCell class membership 
-------------------------------------------------------------------------------    
instance Cellular (MatrixCell v) where
    type Location (MatrixCell v) = (Int,Int)
    type Value (MatrixCell v) = v

    cell loc val = Cell(loc,val)    
    location (Cell (x,_)) = x
    value (Cell (_,a)) = a
        
instance (Formattable v) => Formattable (MatrixCell v) where
    format (Cell ((i,j),a)) = format (i,j,a)
    
instance (Formattable v) => Show (MatrixCell v) where
    show = string . format    
    
testConcat::IO()
testConcat = do
    let m1 = matrix @3 @3 @Int [1..9]
    let m2 = matrix @3 @2 @Int [1..6]
    let m3 = m1 >++< m2
    print m3

