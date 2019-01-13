-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
--{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Structures.Monoid
(
    module X,
    ProductMonoid(..),
    AdditiveMonoid(..)
) where
import Alpha.Canonical.Algebra
import Alpha.Canonical.Structures.Structure as X

import qualified Data.Monoid as Monoid

type MonoidalAlt f a = Monoid.Alt f a
type MonoidalSum a = Monoid.Sum a
type MonoidalProduct a = Monoid.Product a

-- | A multiplicative struture with an identity element    
type ProductMonoid a = (Unital a, Multiplicative a, Eq a) 

-- | A additive struture with an identity element    
type AdditiveMonoid a = (Nullary a, Additive a, Eq a) 

-- Lifts the input into the Alt monoid
-- Example:
-- alt Nothing  <> alt (Just 4) <> alt (Just 7)
-- >> Alt {getAlt = Just 4}
altM::Monoid a => f a -> MonoidalAlt f a
altM = Monoid.Alt

sumM::Monoid a => a -> MonoidalSum a
sumM = Monoid.Sum

prodM::Monoid a => a -> MonoidalProduct a
prodM = Monoid.Product
    