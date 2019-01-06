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
import qualified Data.List as List

type instance Individual (Vector a) = a


instance Container (Vector a)

instance Vacuous (Vector a) where
    empty = Vector.empty
    null = Vector.null
        
instance Singletary (Vector a) where
    singleton a = contain [a]
    