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
    MatrixFacets(..),
    facets,
)
where
import Alpha.Canonical
import Alpha.Linear.Matrix as X

data MatrixFacet 
    = UT
    -- ^ An upper-triangular matrix
    | LT
    -- ^ A lower-triangular matrix
    | Invertible
    -- ^ | A square matrix A such that AB = BA = I for some squre matrix B
    | Orthogonal 
    -- ^ | A real square matrix A such that (A^T)A = A (A^T) = I
    | Unitary 
    -- ^ | A complex square matrix U such that (U^*)U = U(U^*) = I
    | Diagonal
    -- ^ | A matrix A = [a(i,j)] where a(i,j) = 0 if i != j
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


facets = UT :~: LT :~: NoFacets