-----------------------------------------------------------------------------
-- | Abstractions inspired by list-like structure and operations
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Groupable
(
    Groupable(..)
)
where

import Alpha.Base
import Alpha.Canonical.Element
import qualified Data.List as List  

class Groupable c where
    group::(Element c -> Element c -> Bool) -> c -> [[Element c]]
                
instance Groupable [a] where
    group = List.groupBy

