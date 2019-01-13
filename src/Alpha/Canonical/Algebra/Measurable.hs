-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Measurable
(
    Measurable(..),
    
) where
import Alpha.Canonical.Relations

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.MultiSet as Bag
import qualified Data.Set as Set


-- | Characterizes measurable things, in the spirit, but not formally, of Lebesque
class Measurable (n::Nat) a where
    measure::forall b. (Num b) => a -> b

    
instance Length a => Measurable 1 a where
    measure = fromIntegral . length    
        
