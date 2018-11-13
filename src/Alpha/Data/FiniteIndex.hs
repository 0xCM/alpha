-----------------------------------------------------------------------------
-- | Multi-index support
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
module Alpha.Data.FiniteIndex
(
    FiniteMultiIndex
)
where

import Alpha.Base
import Alpha.Data.Product
import Alpha.Data.Natural

type FiniteMultiIndex i j n = UniProduct n (NatRange i j)

type FiniteIndex i j = FiniteMultiIndex i j  1