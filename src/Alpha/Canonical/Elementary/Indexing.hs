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
import Alpha.Base
import Alpha.Canonical.Elementary.Element
import Alpha.Canonical.Elementary.Tuples
import Alpha.Canonical.Elementary.Discrete

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

type instance IndexedElement 1 (a1,a2) = a1
type instance IndexedElement 2 (a1,a2) = a2
type instance IndexedElement 1 (a1,a2,a3) = a1
type instance IndexedElement 2 (a1,a2,a3) = a2
type instance IndexedElement 3 (a1,a2,a3) = a3
type instance IndexedElement 1 (a1,a2,a3,a4) = a1
type instance IndexedElement 2 (a1,a2,a3,a4) = a2
type instance IndexedElement 3 (a1,a2,a3,a4) = a3
type instance IndexedElement 4 (a1,a2,a3,a4) = a4
type instance IndexedElement 1 (a1,a2,a3,a4,a5) = a1
type instance IndexedElement 2 (a1,a2,a3,a4,a5) = a2
type instance IndexedElement 3 (a1,a2,a3,a4,a5) = a3
type instance IndexedElement 4 (a1,a2,a3,a4,a5) = a4
type instance IndexedElement 5 (a1,a2,a3,a4,a5) = a5


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

instance NaturallyIndexed 1 (a1,a2) where
    nix (a1,_) = a1
instance NaturallyIndexed 2 (a1,a2) where
    nix (_,a2) = a2    

instance NaturallyIndexed 1 (a1,a2,a3) where
    nix (a1,_,_) = a1
instance NaturallyIndexed 2 (a1,a2,a3) where
    nix (_,a2,_) = a2        
instance NaturallyIndexed 3 (a1,a2,a3) where
    nix (_,_,a3) = a3            

instance NaturallyIndexed 1 (a1,a2,a3,a4) where
    nix (a1,_,_,_) = a1
instance NaturallyIndexed 2 (a1,a2,a3,a4) where
    nix (_,a2,_,_) = a2        
instance NaturallyIndexed 3 (a1,a2,a3,a4) where
    nix (_,_,a3,_) = a3                
instance NaturallyIndexed 4 (a1,a2,a3,a4) where
    nix (_,_,_,a4) = a4                    

instance NaturallyIndexed 1 (a1,a2,a3,a4,a5) where
    nix (a1,_,_,_,_) = a1
instance NaturallyIndexed 2 (a1,a2,a3,a4,a5) where
    nix (_,a2,_,_,_) = a2        
instance NaturallyIndexed 3 (a1,a2,a3,a4,a5) where
    nix (_,_,a3,_,_) = a3                
instance NaturallyIndexed 4 (a1,a2,a3,a4,a5) where
    nix (_,_,_,a4,_) = a4                        
instance NaturallyIndexed 5 (a1,a2,a3,a4,a5) where
    nix (_,_,_,_,a5) = a5                            