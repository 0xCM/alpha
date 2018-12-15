-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Indexed
(
    Indexed(..),
    IndexedChoice(..),
    splitAt, 
) where
import Alpha.Base
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.Sequence as Sequence
import qualified Data.Map as Map
import Alpha.Canonical.Element

-- | Characterizes a structure of type s holding elements indexed by a value of type i
class Indexed s i where

    at::s -> i -> Element s

    (!)::s -> i -> Element s
    (!) = at            
    infixr 9 !

class SafelyIndexed s i where
    
    lookup::s ->i -> Maybe (Element s)

    (!?)::s -> i -> Maybe (Element s)
    (!?) = lookup
    infixr 9 !?


-- Characterizes a type that represents a finite sequence of mutually disjoint choices
class IndexedChoice a where    
    choiceix::a -> Int

-- | Makes the 'List.genericSplitAt' function the 'default' splitAt function
splitAt::(Integral i) => i -> [a] -> ([a],[a])
splitAt = List.genericSplitAt

instance (Eq a) => Indexed [a] Int where    
    at = (List.!!)
    
instance Indexed (Seq a) Int where
    at = Sequence.index
    
instance Indexed (Vector a) Int where
    at = (Vector.!)
    
instance (Ord k) => Indexed (Map k v) k where
    at map k = (k, map Map.! k)

instance (Ord k) => SafelyIndexed (Map k v) k where
     lookup map k = case (map Map.!? k)of
                        Just v -> Just (k, v)
                        _      -> Nothing

        