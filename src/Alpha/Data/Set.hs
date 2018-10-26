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
    Set
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

instance Enumerable (Set a) a where
    type Source (Set a) a = Set a
    items = Set.toList

instance (Show a) => Formattable (Set a) where
    format x =  wrap (T.pack (show x))
        where wrap y = T.append Ascii.LBrace (T.append y Ascii.RBrace)
    
instance (Hashable a) => Singletary (Set a) a where
    singleton = Set.singleton
    
instance (Eq a, Hashable a) => Existential (Set a) a where
    exists = Set.member
    