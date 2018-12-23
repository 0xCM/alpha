-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Measurable
(
    Measurable(..),
    Length(..),

) where
import Alpha.Canonical.Relations

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.MultiSet as Bag
import qualified Data.Set as Set



-- | Characterizes measurable things, in the spirit, but not formally, of Lebesque
class Measurable (n::Nat) a where
    measure::forall b. (Num b) => a -> b

class Length a where    
    length::a -> Int
        
instance Length a => Measurable 1 a where
    measure = fromIntegral . length    
instance Length [a] where
    length x =  List.length x |> fromIntegral


instance Length Text where
    length t =   Text.length t |> fromIntegral    
instance Length Char where
    length c = 1
        
