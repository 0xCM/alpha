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
    DataRow(..),
    DataCol(..),
    MatrixCell(..),
    cells,
    matrix,
    rowdata,
    coldata,
    indices,
    intmat,
    testConcat,
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
type DataRow n a = Matrix 1 n a

-- | Represents a n x 1 matrix
type DataCol m a = Matrix m 1 a

-- | Represents a cell in a matrix
type MatrixCell v = Cell (Int,Int) v

data MatrixFacet 
    = UpperTriangular 
    | LowerTriangular 
    | Invertible
    -- ^ | A square matrix A such that AB = BA = I for some squre matrix B
    | Orthogonal 
    -- ^| A real square matrix A such that (A^T)A = A (A^T) = I
    | Unitary 
    -- ^| A complex square matrix U such that (U^*)U = U(U^*) = I
    
class KnownNatPair m n => DataGrid m n g where
    -- Extracts an identified row from a gridf
    row::Int -> g -> DataRow n (Individual g)    
    
    -- Extracts an identified column from a grid
    col::Int ->  g -> DataCol m (Individual g)
    
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

-- | Calculates the index values for a (dense) matrix
indices::forall m n. (KnownNatPair m n) => [(Int,Int)]
indices = [(r,c) | r <- natrange @m, c <- natrange @n] where
    (m,n) = (nat @m, nat @n)

-- Creates a test matrix of integer values
intmat::forall m n. (KnownNatPair m n) => Int -> Matrix m n Int
intmat seed = matrix @m @n ((\e -> e + seed) <$> [num 1..num (nat @m * nat @n)])

drow::forall m n a. (KnownNatPair m n) => Int -> Matrix m n a -> DataRow n a
drow i (Matrix m) = m !! i |> list |> matrix @1 @n    

drows::forall m n a. (KnownNatPair m n) => Matrix m n a -> [DataRow n a]
drows m = (\i ->  drow i m) <$> natrange @m

dcol::forall m n a. (KnownNatPair m n) => Int -> Matrix m n a -> DataCol m a
dcol i m = (transpose m) |> drow i |> transpose

dcols::forall m n a. (KnownNatPair m n) => Matrix m n a -> [DataCol m a]
dcols m = (\i ->  dcol i m) <$> natrange @m
      
cells::forall m n a. (KnownNatPair m n) => Matrix m n a -> [Cell (Int,Int) a]
cells m = (\ix -> Cell(ix , m !!ix)) <$> indices @m @n

instance (KnownNatPair m n) => DataGrid m n (Matrix m n a) where
    row = drow
    col = dcol 
    
instance Indexable (Matrix m n a) where
    type Indexer (Matrix m n a) = (Int,Int)
    idx (Matrix m) (i,j) = (m !! i) !! j

instance forall m n p s a. (KnownNatPair m n, KnownNatPair m p, KnownNat (n + p), Eq a) => Concatenable (Matrix m n a) (Matrix m p a) where
    type Concatenated (Matrix m n a) (Matrix m p a)  = Matrix m (n + p) a
    concat a b = newRow <$> natrange @m |> append |> matrix @m @(n+p) where
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

instance forall m n a. (KnownNatPair m n,  Nullary a) => Nullary (Matrix m n a) where
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

instance forall m n a. (KnownNatPair m n, Conjugatable a) => Conjugatable (Matrix m n a) where
    conjugate m = conjugate <$> m

instance (KnownNat n) => NVectored n (DataRow n a) where
    nvector m = individuals m |> vecN

instance (KnownNat n) => NVectored n (DataCol n a) where
    nvector m = individuals m |> vecN
        
instance Formattable a => Formattable (Matrix m n a) where    
    format m =  format <$> (\row -> (pad . format) <$> row ) <$> rows m |> weave EOL |> append where
        rows (Matrix x)  = list <$> list x

instance Formattable a => Show (Matrix m n a) where    
    show = string . format
    

testConcat::IO()
testConcat = do
    let m1 = matrix @3 @3 @Int [1..9]
    let m2 = matrix @3 @2 @Int [1..6]
    let m3 = m1 ++ m2
    print m3

