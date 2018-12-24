-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Invertible
(
    Invertible(..)
)
where
import Alpha.Canonical.Common

-- | Characterizes a type for which an inversion operator is defined
class Invertible a where
    invert::a -> a