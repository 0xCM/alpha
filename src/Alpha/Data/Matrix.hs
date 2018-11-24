module Alpha.Data.Matrix
(
    Matrix, MatrixDim, matrix, mmul
)
where
import qualified Data.Matrix as M
import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Product
import Alpha.Data.Numbers
import Alpha.Data.Natural

-- | Specifies the form of the matrix dimension type
type MatrixDim = (Int, Int)

-- Defines a dimension of arity 2
type family Dim2 (r::Nat) (c::Nat) :: Type where
    -- Nullary dimension
    Dim2 0 0 = NatPair 0 0
    -- Sclar dimension, a matrix with 1 column/row
    Dim2 1 1 = NatPair 1 1
    -- Covector dimension, i.e. a row vector, otherwise known as a 'covector'
    Dim2 1 c = NatPair 1 c
    -- Vector dimension, i.e. a column vector, otherwise known as a 'vector'
    Dim2 r 1 = NatPair r 1
    -- Otherwise
    Dim2 r c = NatPair r c

type family (m1::Dim2 r1 c ) :**: (m2::Dim2 c c2) where
    (a::Dim2 r1 c) :**: (b::Dim2 c c2) = Dim2 r1 c2

-- | Defines a matrix with type-level indexes
newtype Matrix (r::Nat) (c::Nat) a = Matrix (M.Matrix a)    
    deriving (Eq, NFData, Functor, Semigroup, Monoid, Applicative, Foldable, Traversable, Num) 

instance (Show a) => Show(Matrix r c a) where
    show (Matrix m) = show m
    
instance forall r c a. (KnownNat r, KnownNat c, Nullary a, Num a) => Nullary (Matrix r c a) where
    zero = Matrix (M.zero rv cv) where
        rv = int $ natVal $ Proxy @r 
        cv = int $ natVal $ Proxy @c

instance forall r a. (KnownNat r, Nullary a, Num a) => Unital (Matrix r r a) where
    one = Matrix (M.identity rv ) where
        rv = int $ natVal $ Proxy @r 

instance forall r c a. (KnownNat r, KnownNat c) => Dimensional (Matrix r c a) where
    type Dimension (Matrix r c a) = MatrixDim

    dimension (Matrix m) = (rv, cv) where
        rv = int $ natVal $ Proxy @r 
        cv = int $ natVal $ Proxy @c

instance Show a => Formattable (Matrix r c a) where
    format = pack . show

instance Indexed (Matrix r c a) (Int,Int) a where
    item (Matrix m) (r,c) = M.getElem r c m

matrix::forall r c a. [[a]] -> Matrix r c a
matrix g = Matrix $ M.fromLists g  

mmul::forall m n p a. (Num a) => Matrix m n a -> Matrix n p a -> Matrix m p a
mmul (Matrix m1) (Matrix m2) = Matrix $ M.multStd m1 m2

