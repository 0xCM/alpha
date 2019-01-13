-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Metrizable
(
    Metrizable(..),
    MetricSpace(..)
)
where
import Alpha.Canonical.Relations

    -- Characterizes types for which a metric may be defined
class Metrizable a where
    distance::(Num r) => a -> a -> r
    default distance::(Num a, r ~ a) => a -> a -> r
    distance x y = sub' x y |> abs'
    {-# INLINE distance #-}

class Metrizable a => MetricSpace a