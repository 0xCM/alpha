-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Linear.Operators
(
    module X,
    Det(..),
    Cofactor(..),
    LinearOperator(..),
    minor,
    cofactor,
    det,
    rowswap,
    colswap,    
    rowsub,
    colsub,
)
where
import Alpha.Canonical
import Alpha.Linear.Shapes as X
import qualified Data.List as List

-- | Captures a constructed inversion between two squre matrices
type MatrixInversion n a = Inversion (SquareMatrix n a)

-- | Captures a constructed similarity relationship in the following sense:
-- A matrix A is *similar* to a matrix B if there exists an invertible matrix P such that
-- B = (P^{-1})AP. In this case, P is called the *change of basis* matrix
-- See https://en.wikipedia.org/wiki/Matrix_similarity
newtype Similarity n a = Similarity (SquareMatrix n a, SquareMatrix n a, MatrixInversion n a)    

-- | Represents a linear operator over a finite-dimensional vector space
newtype LinearOperator n k = LinearOperator (SquareMatrix n k)
    deriving (Eq,Generic,Data,Typeable)

-- | Represents the data required to compute the determinant of a matrix
newtype Det n a = Det (SquareMatrix n a)
    deriving (Eq, Functor, Foldable, Formattable, Show, Traversable, Generic, Data, Typeable)
    --deriving (Formattable, Show) via (SquareMatrix n a)

-- | Represents the cofactor of square matrix of dimension n + 1
newtype Cofactor n a = Cofactor (Sign, Det n a)
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable) 

-- | Constructs, but does not evaluate, a determinant representation    
det::forall n a.(KnownNat n) => SquareMatrix n a -> Det n a
det = Det

-- Constructs a minor representation from a matrix
minor::forall n a. (KnownNat n, KnownNat (n - 1)) => (Int,Int) -> Matrix n n a -> Det (n - 1) a
minor (i,j) m = minor  where
     keep (Cell ((r,c), _)) = r != i && c != j
     minor = cells m |> filter keep |> (<$>)  (\(Cell(_, val)) -> val) |> matrix |> SquareMatrix |> Det

cofactor::forall n a. (KnownNat n, KnownNat (n - 1)) => (Int,Int) -> Matrix n n a -> Cofactor (n - 1) a
cofactor (i,j) m = Cofactor (sign,minor (i,j) m )  where
     sign  = ifelse ((-1::Int)^( natural(i + j)) == -1) Negative Positive

-- | Swaps the position of a pair of rows
rowswap::forall m n a. (KnownNatPair m n, Eq a) => (Int,Int) -> Matrix m n a -> Matrix m n a
rowswap (i,j) mat 
    = pick <$> natrange @m 
        |> append 
        |> matrix 
    where 
        src = rowdata mat
        ix k = ifelse (k == i) j (ifelse (k == j) i k)
        pick k = src !! (ix k)
        
-- | Swaps the position of a pair of columns
colswap::forall m n a. (KnownNatPair m n, Eq a) => (Int,Int) -> Matrix m n a -> Matrix m n a
colswap (i,j) mat = mat |> transpose |> rowswap (i,j) |> transpose

-- | Substitutes an identified matrix row with another
rowsub::forall m n a. (KnownNatPair m n, Eq a) => Int -> RowMatrix n a -> Matrix m n a -> Matrix m n a
rowsub i r src = pick <$> natrange @m |> append |> matrix where
    rows = rowdata src        
    pick k = ifelse (k == i) (individuals r) (rows !! k) 

-- | Substitutes an identified matrix column with another
colsub::forall m n a. (KnownNatPair m n, Eq a) => Int -> ColMatrix n a -> Matrix m n a -> Matrix m n a
colsub i c src = pick <$> natrange @m |> append |> matrix where
    cols = coldata src        
    pick k = ifelse (k == i) (individuals c) (cols !! k) 

-- rowscale::forall m n a. (KnownNatPair m n, Semiring a) => Int -> a -> Matrix m n a -> Matrix m n a
-- rowscale i s m = undefined where
--     scaled = (row i m)

instance forall m n p a. (KnownNatTriple m n p, Semiring a) => Bimultiplicative  (Matrix m n a) (Matrix n p a) where
    a >*< b = (\(r,c) -> r >*< c) <$> [(r,c) | r <- rows a, c <- cols b] |> matrix

