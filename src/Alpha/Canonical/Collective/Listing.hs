-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Listing
(
    Listing(..),
    Unlisted(..),

) where
import Alpha.Base
import Alpha.Canonical.Element
import qualified Data.Map as Map

class Listing a where
    list::a -> [Element a]
    
class (Listing a) => Unlisted a  where
    unlist::[Element a] -> a

instance Listing (Map a b) where
    list = Map.toList
        
            
        