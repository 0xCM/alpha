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

-- instance (Ord a) => SetSpecBuilder (Vector a) where
--     setspec v = fneSet nel  where
--         items = Vector.toList v 
--         nel = (List.head items) :| (List.tail items) 

instance Vacant (Vector a) where
    empty = Vector.empty
    null = Vector.null
        