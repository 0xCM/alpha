{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Monoidal
(
    Monoidal(..)        
) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Multiplicative
import qualified Data.Monoid as Monoid

type MonoidalAlt f a = Monoid.Alt f a
type MonoidalSum a = Monoid.Sum a
type MonoidalProduct a = Monoid.Product a
-- | A a multiplicative struture with an identity element    
class (Unital a, Multiplicative a, Eq a) => Monoidal a where

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
    

instance Monoidal Natural
instance Monoidal Integer
instance Monoidal Int
instance Monoidal Int8
instance Monoidal Int16
instance Monoidal Int32
instance Monoidal Int64
instance Monoidal Word
instance Monoidal Word8
instance Monoidal Word16
instance Monoidal Word32
instance Monoidal Word64
instance (Integral a, Ord a) => Monoidal (Ratio a)
instance Monoidal Float
instance Monoidal Double
instance Monoidal CFloat
instance  Monoidal CDouble
