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
import qualified Alpha.Data.Asci as Ascii
import Alpha.Canonical
import qualified Data.Set as Set
import Data.Set(Set)

-- Constructs a set from a list
set::Ord a => [a] -> Set a
set = Set.fromList

instance (Show a) => Formattable (Set a) where
    format x =  wrap (T.pack (show x))
        where wrap y = T.append Ascii.LBrace (T.append y Ascii.RBrace)
            
instance (Ord a) => Container (Set a) a where
    type Source (Set a) a = Set a    
    singleton = Set.singleton
    
instance Counted (Set a) where
    count = fromIntegral . Set.size

instance (Ord a) => Unionizable (Set a) where
    union = Set.union

instance (Ord a) => Intersectable (Set a) where
    intersect = Set.intersection

instance (Ord a) => Diffable (Set a) where
    delta = Set.difference
    
        
