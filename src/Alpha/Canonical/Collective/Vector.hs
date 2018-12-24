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

type instance Individual (Vector a) = a


instance Container (Vector a)

instance (Eq a) => SetBuilder (Vector a) a where
    set = Vector.toList                        

instance Vacant (Vector a) where
    empty = Vector.empty
    null = Vector.null
        