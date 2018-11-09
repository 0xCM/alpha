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
    Set, set, Hashable(..)
)
where
import Alpha.Base
import qualified Data.HashSet as Set
import Data.HashSet(HashSet)
import Data.Hashable
import qualified Data.Text as T
import qualified Alpha.Data.Asci as Ascii
import Alpha.Canonical

type Set a = HashSet a

set::(Hashable a, Eq a) => [a] -> Set a
set = Set.fromList

instance (Show a) => Formattable (Set a) where
    format x =  wrap (T.pack (show x))
        where wrap y = T.append Ascii.LBrace (T.append y Ascii.RBrace)

instance Enumerable (Set a) a where
    type Source (Set a) a = Set a
    items = Set.toList
            
instance (Eq a, Hashable a) => Container (Set a) a where
    contains = Set.member
    singleton = Set.singleton
    
