-----------------------------------------------------------------------------
-- | Fundamental constructs
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Alpha.Canonical 
(
    module X


)
where


import Data.Text(Text)
import Data.String(String)
import Data.Int(Int)
import GHC.TypeLits(KnownNat,Nat(..))
import Data.Proxy
import Data.Maybe
import Data.Either
import Data.Ord
import Data.Word
import Data.Bool
import Data.Kind(Type)
import Data.Vector(Vector)
import GHC.Num(Num)
import GHC.Real(Integral)
import Alpha.Canonical.Algebra as X
import Alpha.Canonical.Functors as X
import Alpha.Canonical.Classes as X
import Alpha.Canonical.Operators as X
import Alpha.Canonical.Disjoint as X
import Alpha.Canonical.Families as X


