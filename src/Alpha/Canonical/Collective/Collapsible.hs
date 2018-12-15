module Alpha.Canonical.Collective.Collapsible
(
    Collapsed(..), Collapsible(..),

)
where
import Alpha.Base
import qualified Data.Tree as Tree
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Set as Set

type family Collapsed a

type instance Collapsed (ItemSet (ItemSet a)) = ItemSet a        
type instance Collapsed [[a]] = [a]
type instance Collapsed [a] = a
type instance Collapsed (Tree a) = [a]

-- Removes a layer of structure 
-- In the case of a monoid, 'reduce' reduces to 'fold', pun intended
class Collapsible a where
    collapse::a -> Collapsed a

instance Collapsible (Tree a) where
    collapse = Tree.flatten

instance (Ord a) => Collapsible (ItemSet (ItemSet a)) where
    collapse = Set.unions . toList
        
instance Collapsible [[a]] where
    collapse = List.concat    
    
instance Collapsible [Text] where
    collapse = Text.concat        