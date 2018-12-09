-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Existential
(
    Existential(..)
) where

import Alpha.Base
import Alpha.Canonical.Element
import qualified Data.List as List

class Existential c where
    any::(Element c -> Bool) -> c -> Bool

instance Existential [a] where
    any = List.any
    