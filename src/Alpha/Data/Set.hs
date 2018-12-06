-----------------------------------------------------------------------------
-- | Defines API for set container
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Alpha.Data.Set
(
    Set, set
)
where
import Alpha.Base
import qualified Data.Text as T
import qualified Alpha.Text.Asci as Ascii
import Alpha.Canonical
import qualified Data.Set as S
import Data.Set(Set)

-- Constructs a set from a list
set::Ord a => [a] -> Set a
set = S.fromList

instance (Ord a) => Container (Set a) where
    contain = S.fromList
    contents = S.toList

instance (Show a) => Formattable (Set a) where
    format x =  braces (T.pack (show x))
        where braces y = T.append Ascii.LBrace (T.append y Ascii.RBrace)
            
instance Counted (Set a) where
    count = fromIntegral . S.size

instance (Ord a) => Setwise (Set a) where
    union = S.union
    intersect = S.intersection
    delta  = S.difference 

instance (Ord a) => Filterable (Set a) where
    filter = S.filter

    
instance (Ord a) => Collapsible (Set (Set a)) where
    type Collapsed (Set (Set a)) = Set a
    collapse = S.unions . toList

-- instance (Ord a) => Membership (Set a) where
--     type Member (Set a) = a
--     member = S.member
    
instance (Ord a) => FiniteContainer (Set a) where
    
instance (Ord a) => FiniteSet (Set a) where