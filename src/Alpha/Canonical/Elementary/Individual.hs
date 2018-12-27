-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Elementary.Individual
(
    Individual(..),
    Membership(..),
) where
import Alpha.Canonical.Common

type family Individual a
type instance Individual [a] = a
type instance Individual Integer = Integer
type instance Individual Int = Int
type instance Individual Int8 = Int8
type instance Individual Int16 = Int16
type instance Individual Int32 = Int32
type instance Individual Int64 = Int64
type instance Individual (Ratio a) = Ratio a
type instance Individual Natural = Natural
type instance Individual Word = Word
type instance Individual Word8 = Word8
type instance Individual Word16 = Word16
type instance Individual Word32 = Word32
type instance Individual Word64 = Word64

class Membership a where
    members::a -> [Individual a]
