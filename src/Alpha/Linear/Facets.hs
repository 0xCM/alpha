-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Linear.Facets
(
    module X,
    MatrixFacet(..),
    MatrixFacetMoniker(..),
    MatrixFacets(..),
    RealMatrix(..), ComplexMatrix(..),
    Square(..),
    Singular(..),
    Invertible(..),
    Triangular(..),
    LowerTriangular(..),
    UpperTriangular(..),    
    SymmetricMatrix(..), HermetianMatrix(..),
    DiagonalMatrix(..),
    Definite(..),
    SemiDefinite(..),
    RealDefinite(..), ComplexDefinite(..),
    RealPositiveDefinite(..), ComplexPositiveDefinite(..),
    RealNegativeDefinite(..), ComplexNegativeDefinite(..),
    RealPositiveSemiDefinite(..), ComplexPositiveSemiDefinite(..),
    RealNegativeSemiDefinite(..), ComplexNegativeSemiDefinite(..),
    RealSemiDefinite(..), ComplexSemiDefinite(..),
    Orthogonal(..), Unitary(..),

    facets,
)
where
import Alpha.Canonical hiding(Symmetric,Invertible)
import Alpha.Linear.Matrix as X

-- | Defines the root of the matrix classification heirarchy
class MatrixFacet a where

-- | Classifies a matrix with real entries
class MatrixFacet a => RealMatrix a where

-- | Classifies a matrix with complex entries
class MatrixFacet a => ComplexMatrix a where

-- | Classifies a square matrix
class MatrixFacet a => Square a where

-- | Classifies a square matrix for which no inverse exists
-- See https://en.wikipedia.org/wiki/Invertible_matrix
class Square a => Singular a where

-- | Classifies a square matrix for which an inverse exists
-- See https://en.wikipedia.org/wiki/Invertible_matrix
class Square a => Invertible a where

-- | Classifies a triangular matrix
-- See https://en.wikipedia.org/wiki/Triangular_matrix
class Square a => Triangular a where

-- | Classifies a lower-triangular matrix
-- See https://en.wikipedia.org/wiki/Triangular_matrix
class Triangular a => LowerTriangular a where

-- | Classifies an upper-triangular matrix
-- See https://en.wikipedia.org/wiki/Triangular_matrix
class Triangular a => UpperTriangular a where

-- | Classifies a matrix that is invariant under transpostion
-- See https://en.wikipedia.org/wiki/Symmetric_matrix
class Square a => SymmetricMatrix a where

-- | Classifies a complex matrix that is invariant under conjugate transposition
-- See https://en.wikipedia.org/wiki/Hermitian_matrix
class (Square a, ComplexMatrix a) => HermetianMatrix a where
    
-- | Classifies a diagonal matrix
-- https://en.wikipedia.org/wiki/Diagonal_matrix
class (LowerTriangular a, UpperTriangular a) => DiagonalMatrix a where

-- | Defines root of definite classification hierarchy
class Square a => Definite a where

-- | Defines root of real definite classification hierarchy
-- See https://en.wikipedia.org/wiki/Definiteness_of_a_matrix
class (RealMatrix a, Definite a, SymmetricMatrix a)  => RealDefinite a where    

-- | Defines root of complex definite classification hierarchy
-- See https://en.wikipedia.org/wiki/Definiteness_of_a_matrix
class (ComplexMatrix a, Definite a, HermetianMatrix a) => ComplexDefinite a where

-- | Classifies a semi-definite matrix
-- See https://en.wikipedia.org/wiki/Definiteness_of_a_matrix
class Definite a => SemiDefinite a where

-- | Classifies a complex positive-definite matrix
-- See https://en.wikipedia.org/wiki/Definiteness_of_a_matrix
class ComplexDefinite a => ComplexPositiveDefinite a where

-- | Classifies a complex negative-definite matrix
-- See https://en.wikipedia.org/wiki/Definiteness_of_a_matrix    
class ComplexDefinite a => ComplexNegativeDefinite a where    

-- | Classifies a complex semi-definite matrix
-- See https://en.wikipedia.org/wiki/Definiteness_of_a_matrix
class ComplexDefinite a => ComplexSemiDefinite a where    

-- | Classifies a complex negative-semi-definite matrix
-- See https://en.wikipedia.org/wiki/Definiteness_of_a_matrix
class ComplexSemiDefinite a => ComplexNegativeSemiDefinite a where    

-- | Classifies a complex positive-semi-definite matrix
-- See https://en.wikipedia.org/wiki/Definiteness_of_a_matrix
class ComplexSemiDefinite a => ComplexPositiveSemiDefinite a where

-- | Classifies a real positive-definite matrix
-- See https://en.wikipedia.org/wiki/Definiteness_of_a_matrix
class RealDefinite a => RealPositiveDefinite a where

-- | Classifies a real negative-definite matrix
-- See https://en.wikipedia.org/wiki/Definiteness_of_a_matrix    
class RealDefinite a => RealNegativeDefinite a where    

-- | Classifies a Real semi-definite matrix
-- See https://en.wikipedia.org/wiki/Definiteness_of_a_matrix
class (RealDefinite a, SemiDefinite a) => RealSemiDefinite a where    

-- | Classifies a real positive-semi-definite matrix
-- See https://en.wikipedia.org/wiki/Definiteness_of_a_matrix        
class RealDefinite a => RealPositiveSemiDefinite a where    

-- | Classifies a real negative-semi-definite matrix
-- See https://en.wikipedia.org/wiki/Definiteness_of_a_matrix    
class RealDefinite a => RealNegativeSemiDefinite a where    

-- | Classifies a real matrix whose inverse is equal to its transpose
-- See https://en.wikipedia.org/wiki/Orthogonal_matrix
class RealMatrix a => Orthogonal a where
    
-- | Classifies a complex matrix whose inverse is equal to its cojugate transpose
-- See https://en.wikipedia.org/wiki/Orthogonal_matrix
class ComplexMatrix a => Unitary a where
    
data MatrixFacetMoniker 
    = UpperTriangular
    -- ^ An upper-triangular matrix
    | LowerTriangular
    -- ^ A lower-triangular matrix
    | Invertible
    -- ^ | A square matrix A such that AB = BA = I for some squ matrix B
    | Orthogonal 
    -- ^ | A real square matrix A such that (A^T)A = A (A^T) = I
    | Unitary 
    -- ^ | A complex square matrix U such that (U^*)U = U(U^*) = I
    | Diagonal
    -- ^ | A matrix A = [a(i,j)] where a(i,j) = 0 if i != j
    | Symmetric
    -- ^ | A square matrix A such that A^T = A
    | Hermetian
        deriving (Show,Eq,Ord,Enum)

-- Abstract row representative
data family MatrixFacets (xs::[Type])

data instance MatrixFacets '[] = NoFacets
data instance MatrixFacets (x ': xs) = x :~: MatrixFacets xs
infixr 2 :~:

deriving instance Eq (MatrixFacets '[])
deriving instance (Eq x, Eq (MatrixFacets xs)) => Eq (MatrixFacets (x ': xs))

deriving instance Ord (MatrixFacets '[])
deriving instance (Ord x, Ord (MatrixFacets xs)) => Ord (MatrixFacets (x ': xs))

instance Show (MatrixFacets '[]) where
    show _ = "[]"

instance (Show e, Show (MatrixFacets r)) => Show (MatrixFacets(e ': r)) where
    show (x :~: l)  = let '[':s = show l 
                            in "[" <> show x <> (if s == "]" then s else "," <> s)


facets = UpperTriangular :~: LowerTriangular :~: NoFacets