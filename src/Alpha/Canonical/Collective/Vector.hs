-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Vector where

import Alpha.Canonical.Elementary
import Alpha.Canonical.Collective.Container

import qualified Data.Vector as Vector

type instance Element (Vector a) = a
instance (Eq a) => Structure (Vector a) where
    type Individual (Vector a) = Element (Vector a)

instance Container (Vector a)

instance (Eq a) => Discrete (Vector a) where
    members = Vector.toList                        

instance Vacant (Vector a) where
    empty = Vector.empty
    null = Vector.null
        