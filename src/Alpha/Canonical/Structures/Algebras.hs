-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Structures.Algebras
(
    Algebra(..)
) where
import Alpha.Canonical.Algebra
import Alpha.Canonical.Structures.VectorSpace

-- | Characterizes an algebra 'a' over a field 'k'
-- See https://en.wikipedia.org/wiki/Algebra_over_a_field
class VectorSpace k a => Algebra k a where

    -- | Represents multiplication of algebras and is required
    -- to be both left/right distributive and be compatible
    -- with the scalar multiplication operation defined over
    -- the underlying field in the following sense:
    -- (a*x)<&>(b*y) = (a*b)(x<&>y) for all scalars a, b and
    -- vectors x, y
    -- Furthermore, in general, <&> is not necessarily associative
    (<&>)::a -> a -> a

instance Structure 2 Algebra