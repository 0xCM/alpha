module Alpha.Canonical.Elementary.Sets
(
    Subsets(..),
    SetAlgebra(..),
    SigmaAlgebra(..),
) where
import Alpha.Canonical.Elementary.Set


-- | Represents a collection of subsets
newtype Subsets a = Subsets (Set (Set a))

-- | A sigma algebra over a set A, σ(A), is a set of elements from
-- the powerset of a, P(A), where
-- 1) A is in σ(A)
-- 2) The union of a countable number of elements in σ(A) 
-- is also in σ(A)
-- 3) The complement of any element of σ(A) is also in σ(A)
-- See Y2013PTCC
class SigmaAlgebra a where

-- | An algebra over a set A is a collection of subsets of
-- A that contains A itself and is closed with respect to
-- set difference and finite intersection
-- See Y2013PTCC
newtype SetAlgebra a = SetAlgebra (Subsets a)

-- | A ring over a set A is a collection of subsets of
-- A that contains the empty set and is closed with respect
-- to set difference and finite union
-- See Y2013PTCC
newtype SetRing a = SetRing (Subsets a)

-- | A semiring R over a set A is a collection of subsets of
-- a that contains the empty, is closed with respect to finite
-- intersection and the difference between any two elements 
-- in R can be expressed as a finite union of mutually disjoint
-- elements in R
newtype SetSemiring a = SetSemiring (Subsets a)