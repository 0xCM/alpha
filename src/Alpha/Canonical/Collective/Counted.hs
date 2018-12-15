module Alpha.Canonical.Collective.Counted
(
    Counted(..)
)
where
import Alpha.Base
import Alpha.Canonical.Operators
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.MultiSet as Bag
import qualified Data.Set as Set
    
-- | Defines membership predicated on the ability to be counted by an existential machine
class Counted a where
    -- | Counts the number of items within the purview of the subject
    count::(Integral n) => a -> n


instance Counted [a] where
    count = fromIntegral . List.length
    
instance Counted (Bag e) where
    count = fromIntegral . Bag.size
    
instance Counted (ItemSet a) where
    count = fromIntegral . Set.size
    
    