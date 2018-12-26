-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Elementary.Indexing
(
    IndexedElement(..),
    Indexed(..),
    NaturallyIndexed(..),
    SafeIndex(..),

)
where
import Alpha.Canonical.Common



import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.Sequence as Sequence
    
type family IndexedElement (i::k) a
type instance IndexedElement a (Map a b) = (a,b)
type instance IndexedElement Int (Vector a) = a
type instance IndexedElement Int (Seq a) = a
type instance IndexedElement Int [a] = a



-- | Characterizes a structure of type s holding elements indexed by a value of type i
class Indexed i a where

    at::a -> i -> IndexedElement i a

    (!)::a -> i -> IndexedElement i a
    (!) = at            
    infixr 9 !

class SafeIndex s i where
    
    lookup::s -> i -> Maybe (IndexedElement i s)

    (!?)::s -> i -> Maybe (IndexedElement i s)
    (!?) = lookup
    infixr 9 !?

class KnownNat i => NaturallyIndexed i a where
    nix::a -> IndexedElement i a

                    
instance (Eq a) => Indexed Int [a] where    
    at = (List.!!)
    
instance Indexed Int (Seq a) where
    at = Sequence.index
    
instance Indexed Int (Vector a) where
    at = (Vector.!)
    
instance (Ord k) => Indexed k (Map k v) where
    at map k = (k, map Map.! k)

instance (Ord k) => SafeIndex (Map k v) k where
     lookup map k = case (map Map.!? k)of
                        Just v -> Just (k, v)
                        _      -> Nothing

