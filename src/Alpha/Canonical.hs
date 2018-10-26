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
    module Alpha.Canonical.Algebra,
    module Alpha.Canonical.Functors,
    module Alpha.Canonical.Classes,
    module Alpha.Canonical.Operators,
    Lazy(..), Eager(..), Boxed(..), Raw(..)

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
import Alpha.Canonical.Algebra
import Alpha.Canonical.Functors
import Alpha.Canonical.Classes
import Alpha.Canonical.Operators


type family Lazy a 

type family Eager a

type family Boxed a

type family Raw a
