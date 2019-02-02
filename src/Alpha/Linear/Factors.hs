-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Linear.Factors
(
    LTM(..),
    UTM(..),
    LU(..)
)
where
import Alpha.Canonical
import Alpha.Linear.Matrix as X
import Alpha.Linear.Shapes as X
    
-- | Represents a lower-triangular matrix
type LTM n a = SquareMatrix n a

-- | Represents an upper-triangular matrix
type UTM n a = SquareMatrix n a

-- | Represents an LU matrix factorization
-- See https://arxiv.org/pdf/math/0506382.pdf for a description of the matrices for which an LU factorization exists
newtype LU n a = LU (SquareMatrix n a, LTM n a, UTM n a)


