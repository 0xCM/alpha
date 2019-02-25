-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra(module X, kdelta) where
import Alpha.Canonical.Common as X
import Alpha.Canonical.Relations as X
import Alpha.Canonical.Algebra.Action as X
import Alpha.Canonical.Algebra.Additive as X
import Alpha.Canonical.Algebra.Based as X
import Alpha.Canonical.Algebra.Cosets as X
import Alpha.Canonical.Algebra.Divisive as X
import Alpha.Canonical.Algebra.Distributive as X
import Alpha.Canonical.Algebra.Metrizable as X
import Alpha.Canonical.Algebra.Modular as X
import Alpha.Canonical.Algebra.Multiplicative as X
import Alpha.Canonical.Algebra.Negatable as X
import Alpha.Canonical.Algebra.Numeric as X
import Alpha.Canonical.Algebra.Span as X
import Alpha.Canonical.Algebra.Series as X
import Alpha.Canonical.Algebra.Subtractive as X
import Alpha.Canonical.Algebra.Successive as X
import Alpha.Canonical.Algebra.Unital as X
import Alpha.Canonical.Algebra.Powers as X


kdelta::(Eq a, Nullary a, Unital a) => a -> a -> a
kdelta i j = ifelse (i == j) one zero 
