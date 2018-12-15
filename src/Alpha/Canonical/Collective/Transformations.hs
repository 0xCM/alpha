-----------------------------------------------------------------------------
-- | Abstractions inspired by list-like structure and operations
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Transformations
(
    Zippable(..), Groupable(..)
) where

import Alpha.Base
import Alpha.Canonical.Element
import qualified Data.List as List

-- Characterizes a composition and pairing of heterogenous values
class Zippable a b c where
    
    -- The left source type
    type LeftZip a b c

    -- The right source type
    type RightZip a b c

    -- The result type
    type Zipped a b c

    -- The combining operator
    type Zipper a b c
    
    zip::Zipper a b c -> LeftZip a b c -> RightZip a b c -> Zipped a b c
    
class Groupable c where
    group::(Element c -> Element c -> Bool) -> c -> [[Element c]]
                
instance Groupable [a] where
    group = List.groupBy

    
instance Zippable [a] [b] [c] where
    type LeftZip [a] [b] [c] = [a]
    type RightZip [a] [b] [c] = [b]
    type Zipped [a] [b] [c] = [c]    
    type Zipper [a] [b] [c] = a -> b -> c

    zip = List.zipWith



