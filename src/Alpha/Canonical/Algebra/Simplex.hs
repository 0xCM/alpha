module Alpha.Canonical.Algebra.Simplex
(
    Simplex(..),
    SimplicalComplex(..)
)
where
import Alpha.Canonical.Relations
-- This development foolows the interpretation of a simplex as defined by Y2008DAT
-- which states that:
-- A simplicial complex K := (E,S) consists of a set 
-- E of vertices and a set S of finite non-empty subsets of E. 
-- An element s ∊ S with q + 1 elements is called a *q-simplex* 
-- of K provided the following axioms hold:
-- (1) The singleton {e} ∊ S for each e ∊ E.
-- (2) If t ∊ S and s ⊂ t is non-empty, then s ∊ S

newtype Simplex e = Simplex (Int, NonEmpty e)

newtype SimplicalComplex e = SimplicalComplex ([e], [Simplex e])