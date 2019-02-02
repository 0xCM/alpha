-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Linear.Shapes
(
    module X,
    ColMatrix(..),
    AsColMatrix(..),
    RowMatrix(..),
    AsRowMatrix(..),    
    SquareMatrix(..),
    row,rows,col,cols,
) where
import Alpha.Canonical
import Alpha.Linear.Matrix as X
import Alpha.Linear.Grid as X
import qualified Data.List as List

-- | Represents a square matrix
newtype SquareMatrix n a = SquareMatrix (Matrix n n a)    
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable)
    deriving (Indexable, Discrete)
    deriving (Show,Formattable) via (Matrix n n a)
type instance Individual (SquareMatrix n a) = a
type instance Multiplied (SquareMatrix n a) (SquareMatrix n a) = SquareMatrix n a

instance Newtype (SquareMatrix n a)
deriving instance KnownNat n => Dimensional (SquareMatrix n a)
deriving instance KnownNat n => Transposable (SquareMatrix n a)
deriving instance (KnownNat n,  Nullary a) => Nullary (SquareMatrix n a)
deriving instance (KnownNat n,  Unital a, Nullary a) => Unital (SquareMatrix n a)
deriving instance (KnownNat n,  Additive a) => Additive (SquareMatrix n a)
deriving instance (KnownNat n,  Subtractive a) => Subtractive (SquareMatrix n a)
deriving instance (KnownNat n,  Negatable a) => Negatable (SquareMatrix n a)
deriving instance (KnownNat n,  AbelianGroup a) => AbelianGroup (SquareMatrix n a)
deriving instance (KnownNat n, LeftAction k a) => LeftAction k (SquareMatrix n a)

-- | Represents a matrix with a single column
newtype ColMatrix n a = ColMatrix (Matrix n 1 a)
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable)
    deriving (Discrete)
    deriving (Show,Formattable) via (Matrix n 1 a)

type instance Individual (ColMatrix m a) = a

-- | Characterizes a type from which a row column matrix can be constructed
class (KnownNat n) => AsColMatrix n a where
    colmatrix::a -> ColMatrix n (Individual a)

-- | Represents a matrix with a single row
newtype RowMatrix n a = RowMatrix (Matrix 1 n a)
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable)
    deriving (Discrete)
    deriving (Show,Formattable) via (Matrix 1 n a)
type instance Individual (RowMatrix n a) = a


-- | Characterizes a type from which a row matrix can be constructed
class (KnownNat n) => AsRowMatrix n a where
    rowmatrix::a -> RowMatrix n (Individual a)

row::forall m n a. (KnownNatPair m n, Eq a) => Int -> Matrix m n a -> RowMatrix n a
row i src = rowdata src !! i |> matrix |> RowMatrix

rows::forall m n a. (KnownNatPair m n, Eq a) => Matrix m n a -> [RowMatrix n a]
rows src = (\i -> row i src) <$> natrange @m

col::forall m n a. (KnownNatPair m n, Eq a) => Int -> Matrix m n a -> ColMatrix m a
col i src = coldata src !! i |> matrix |> ColMatrix

cols::forall m n a. (KnownNatPair m n, Eq a) => Matrix m n a -> [ColMatrix m a]
cols src = (\i -> col i src) <$> natrange @n

    
instance forall n a. (KnownNat n) => AsRowMatrix n [a] where
    rowmatrix src = matrix src |> RowMatrix    

instance forall n a. (KnownNat n) => AsColMatrix n [a] where
    colmatrix src = matrix src |> ColMatrix
    
instance Indexable (ColMatrix n a) where
    type Indexer (ColMatrix n a) = Int
    idx (ColMatrix m) i = m !! (i,0)

instance forall m n p a. (KnownNatPair m n, KnownNat (n + m), Eq a) => Concatenable (ColMatrix m a) (ColMatrix n a) where
    type Concatenated (ColMatrix m a) (ColMatrix n a)  = ColMatrix (n + m) a
    a ++ b = (individuals a) ++ (individuals b) |> colmatrix
        
instance Newtype (ColMatrix m a)
deriving instance KnownNat m => Dimensional (ColMatrix m a)
deriving instance (KnownNat m,  Nullary a) => Nullary (ColMatrix m a)
deriving instance (KnownNat m,  Additive a) => Additive (ColMatrix m a)
deriving instance (KnownNat m,  Subtractive a) => Subtractive (ColMatrix m a)
deriving instance (KnownNat m,  Negatable a) => Negatable (ColMatrix m a)
deriving instance (KnownNat m,  AbelianGroup a) => AbelianGroup (ColMatrix m a)
deriving instance (KnownNat m, LeftAction k a) => LeftAction k (ColMatrix m a)    

instance Indexable (RowMatrix n a) where
    type Indexer (RowMatrix n a) = Int
    idx (RowMatrix m) i = m !! (0,i)

instance forall m n p a. (KnownNatPair m n, KnownNat (n + m), Eq a) => Concatenable (RowMatrix m a) (RowMatrix n a) where
    type Concatenated (RowMatrix m a) (RowMatrix n a)  = RowMatrix (n + m) a
    a ++ b = (individuals a) ++ (individuals b) |> rowmatrix
        
instance Newtype (RowMatrix n a)
deriving instance KnownNat n => Dimensional (RowMatrix n a)
deriving instance (KnownNat n,  Nullary a) => Nullary (RowMatrix n a)
deriving instance (KnownNat n,  Additive a) => Additive (RowMatrix n a)
deriving instance (KnownNat n,  Subtractive a) => Subtractive (RowMatrix n a)
deriving instance (KnownNat n,  Negatable a) => Negatable (RowMatrix n a)
deriving instance (KnownNat n,  AbelianGroup a) => AbelianGroup (RowMatrix n a)
deriving instance (KnownNat n, LeftAction k a) => LeftAction k (RowMatrix n a)

type instance Multiplied (RowMatrix n a) (ColMatrix m a) = a

instance forall n a. (KnownNat n, Semiring a) => Bimultiplicative (RowMatrix n a) (ColMatrix n a) where
    x >*< y = reduce zero (+) z where
        z = (\i -> (x !! i) * (y !! i) ) <$> (natrange @n)        

instance forall n a. KnownNat n => Transposable (ColMatrix n a) where
    type Transposed (ColMatrix n a) = RowMatrix n a
    transpose = rowmatrix . individuals
        
instance forall n a. KnownNat n => Transposable (RowMatrix n a) where
    type Transposed (RowMatrix n a) = ColMatrix n a
    transpose = colmatrix . individuals
    
        