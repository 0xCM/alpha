-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Measurable
(
    Measurable(..),
    Dimension(..), Dimensional(..),    
    Length(..),
    

) where
import Alpha.Base
import Alpha.Canonical.Functions

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.MultiSet as Bag
import qualified Data.Set as Set

type family Dimension a

-- | Characterizes measurable things, in the spirit, but not formally, of Lebesque
class Measurable (n::Nat) a where
    measure::forall b. (Num b) => a -> b

-- Characterizes a type for which a notion of dimensionality 
-- can be defined, e.g., an array, matrix or more generally a tensor
class Dimensional a where
    dimension::a -> Dimension a

class Length a where    
    length::forall b. (Num b) => a -> b
        
instance Length a => Measurable 1 a where
    measure = length    
instance Length [a] where
    length x = List.length x |> fromIntegral


instance Length Text where
    length t =   Text.length t |> fromIntegral    
instance Length Char where
    length c = 1
        
