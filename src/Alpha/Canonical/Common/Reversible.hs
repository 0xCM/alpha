-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Reversible
(    
    Reversible(..),
    Flippable(..)
    
) where
--import Alpha.Canonical.Relations
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Types
import qualified Data.List as List  
import qualified Data.Map as Map
import qualified Data.Text as Text

-- | Characterizes a type that manifests the concept
-- of an invertible reversion    
class Reversible a b | a -> b, b -> a  where
    reverse::a -> b

class Flippable a where
    type Flipped a    
    flip::a -> Flipped a

instance (Ord a, Ord b) => Flippable (Map a b) where
    type Flipped (Map a b) = Map b a
    flip m = Map.toList m |> fmap (\(y,z) -> (z,y)) |> Map.fromList
    
instance Flippable (a -> b -> c) where
    type Flipped (a -> b -> c) = b -> a -> c
    flip = flip'
        
instance Reversible [a] [a] where
    reverse = List.reverse
    
instance Reversible Text Text where    
    reverse = Text.reverse
    
instance Reversible (a1,a2) (a2,a1) where
    reverse (a1, a2) = (a2, a1)
instance Reversible (a1, a2, a3) (a3, a2, a1) where
    reverse (a1, a2, a3) = (a3, a2, a1)
instance Reversible (a1, a2, a3, a4) (a4, a3, a2, a1) where
    reverse (a1, a2, a3, a4) = (a4, a3, a2, a1)
instance Reversible (a1, a2, a3, a4, a5) (a5, a4, a3, a2, a1) where
    reverse (a1, a2, a3, a4, a5) = (a5, a4, a3, a2, a1)

