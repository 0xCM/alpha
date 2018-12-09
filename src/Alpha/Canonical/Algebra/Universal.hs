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
import qualified Data.List as List

class Universal c where
    all::(Element c -> Bool) -> c -> Bool

instance Universal [a] where
    all = List.all
    