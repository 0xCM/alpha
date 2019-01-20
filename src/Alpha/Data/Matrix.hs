-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
module Alpha.Data.Matrix
(
    Matrix(..),
    MatrixCell(..),
    SquareMatrix(..),
    Det(..),
    Cofactor(..),
    DataRow(..),
    DataCol(..),
    matrix,
    kdelta,
    indices,
    tmatrix,
    cells,
    minor,
    cofactor,
)
where
-- TODO Define a (left|right) action using a permutation on a matrix to change (rows|columns)
import Alpha.Canonical
import qualified Data.List as List

-- | Represents a matrix
newtype Matrix m n a = Matrix (VecN m (VecN n a))
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable) 

type instance Individual (Matrix m n a) = a

-- | Represents a cell in a matrix
type MatrixCell v = Cell (Int,Int) v

-- | Represents a square matrix
type SquareMatrix n a = Matrix n n a

-- | Represents a 1 x n matrix
type DataRow n a = Matrix 1 n a

-- | Represents a n x 1 matrix
type DataCol m a = Matrix m 1 a

-- | Represents the data required to compute the determinant of a matrix
newtype Det n a = Det (SquareMatrix n a)
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable) 

-- | Represents the cofactor of square matrix of dimension n + 1
newtype Cofactor n a = Cofactor (Sign, Det n a)
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable) 

instance Cellular (MatrixCell v) where
    type Location (MatrixCell v) = (Int,Int)
    type Value (MatrixCell v) = v

    cell loc val = Cell(loc,val)    
    location (Cell (x,_)) = x
    value (Cell (_,a)) = a

-- Constructs a matrix from a list where the elements are in row-major order
matrix::forall m n a. (KnownNat m, KnownNat n) => [a] -> Matrix m n a
matrix rowmajor = Matrix $ vecN @m rows where
    (m,n) = (nat @m, nat @n)
    segments = segment n rowmajor
    rows = (vecN @n) <$> segments

kdelta::(Eq a, Nullary a, Unital a) => a -> a -> a
kdelta i j = ifelse (i == j) one zero 

indices::forall m n. (KnownNatPair m n) => [(Int,Int)]
indices = [ (r,c) | r <- [0..(m-1)], c <- [0..(n-1)]] where
    (m,n) = (nat @m, nat @n)

cells::forall m n a. (KnownNatPair m n) => Matrix m n a -> [Cell (Int,Int) a]
cells m = (\ix -> Cell(ix , m !!ix)) <$> indices @m @n

rowvecs::forall m n a. (KnownNatPair m n) => Matrix m n a -> [VecN n a]
rowvecs (Matrix (VecN v))  = list v

colvecs::forall m n a. (KnownNatPair m n) => Matrix m n a -> [VecN m a]
colvecs m  = transpose m |> rowvecs
    
-- Creates a test matrix of integer values
tmatrix::forall m n. (KnownNatPair m n) => Int -> Matrix m n Int
tmatrix seed = matrix @m @n ((\e -> e + seed) <$> [num 1..num (nat @m * nat @n)])

-- Extracts an identified row from a matrix
row::forall m n a. (KnownNatPair m n) => Int ->  Matrix m n a -> DataRow n a
row i (Matrix m) = m !! i |> list |> matrix @1 @n

-- Extracts an identified column from a matrix
col::forall m n a. (KnownNatPair m n) => Int ->  Matrix m n a -> DataCol m a
col i m = transpose m |> row i |> transpose

-- Construct a minor representation from a matrix
minor::forall n a. (KnownNat n, KnownNat (n - 1)) => (Int,Int) -> SquareMatrix n a -> Det (n - 1) a
minor (i,j) m = minor  where
     keep (Cell ((r,c), _)) = r != i && c != j
     minor = cells m |> List.filter keep |> (<$>)  (\(Cell(_, val)) -> val) |> matrix |> Det

cofactor::forall n a. (KnownNat n, KnownNat (n - 1)) => (Int,Int) -> SquareMatrix n a -> Cofactor (n - 1) a
cofactor (i,j) m = Cofactor (sign,minor (i,j) m )  where
     sign  = ifelse ((-1::Int)^( natural(i + j)) == -1) Negative Positive
     
instance forall m n a. (KnownNat m, KnownNat n) => Indexable (Matrix m n a) where
    type Indexer (Matrix m n a) = (Int,Int)
    idx (Matrix m) (i,j) = (m !! i) !! j
    
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
    format m =  format <$> (\row -> (pad . format) <$> row ) <$> rows m |> weave EOL |> append

instance forall m n a. (KnownNat m, KnownNat n, Formattable a) => Show (Matrix m n a) where    
    show = string . format

instance forall m n a. (KnownNat m, KnownNat n,  Nullary a) => Nullary (Matrix m n a) where
    zero = matrix @m @n (clone count zero) 
        where count = natmul @m @n
            
instance forall n a. (KnownNat n, Unital a, Nullary a) => Unital (Matrix n n a) where
    one = matrix @n @n [ifelse (i == j) one zero | i <- [1..n], j <- [1..n]] where
        n = natg @n

instance forall m n a. (KnownNat m, KnownNat n,  Additive a) => Additive (Matrix m n a) where
    add m1 m2 = matrix @m @n [ m1 !! (r,c) + m2 !! (r,c) | (r,c) <- indices @m @n]

instance forall m n a. (KnownNat m, KnownNat n,  Subtractive a) => Subtractive (Matrix m n a) where
    sub m1 m2 = matrix @m @n [ m1 !! (r,c) - m2 !! (r,c) | (r,c) <- indices @m @n]               

instance forall m n a. (KnownNat m, KnownNat n, Negatable a) => Negatable (Matrix m n a) where
    negate m = negate <$> m
        
instance forall m n a. (KnownNat m, KnownNat n, AbelianGroup a) => AbelianGroup (Matrix m n a) where

instance forall m n k a. (KnownNat m, KnownNat n, LeftAction k a) => LeftAction k (Matrix m n a) where
    k *. m = (\item -> k *. item) <$> m
    
instance forall m n a k. (KnownNat m, KnownNat n, RightAction a k) => RightAction (Matrix m n a) k where
    m .* k  = (\item -> item .* k) <$> m
                            
instance (Formattable v) => Formattable (MatrixCell v) where
    format (Cell ((i,j),a)) = format (i,j,a)
    
instance (Formattable v) => Show (MatrixCell v) where
    show = string . format    