-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Universal
(
    Universal(..)
) where

import Alpha.Base
import Alpha.Canonical.Element
import Alpha.Canonical.Operators

import qualified Data.List as List
import qualified Data.Set as Set

class Universal c where
    all::(Element c -> Bool) -> c -> Bool

instance Universal [a] where
    all = List.all
    
