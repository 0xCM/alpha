-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Elementary.Indexed
(
    IndexedElement(..),
    IndexedFamily(..),
    Indexed(..),
    MultiIndex(..),
    MultiIndexed(..),
    IndexRange(..),    
    NaturallyIndexed(..),
    SafeIndex(..),
    IndexedTerm(..), 
    ixrange,
    term,
)
where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.Set

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

type instance IndexedElement 1 (UniTuple1 a) = a
type instance IndexedElement 1 (Tuple1 a1) = a1
type instance IndexedElement 1 (Tuple2 a1 a2) = a1
type instance IndexedElement 2 (Tuple2 a1 a2) = a2
type instance IndexedElement 1 (Tuple3 a1 a2 a3) = a1
type instance IndexedElement 2 (Tuple3 a1 a2 a3) = a2
type instance IndexedElement 3 (Tuple3 a1 a2 a3) = a3
type instance IndexedElement 1 (Tuple4 a1 a2 a3 a4) = a1
type instance IndexedElement 2 (Tuple4 a1 a2 a3 a4) = a2
type instance IndexedElement 3 (Tuple4 a1 a2 a3 a4) = a3
type instance IndexedElement 4 (Tuple4 a1 a2 a3 a4) = a4
type instance IndexedElement 1 (Tuple5 a1 a2 a3 a4 a5) = a1
type instance IndexedElement 2 (Tuple5 a1 a2 a3 a4 a5) = a2
type instance IndexedElement 3 (Tuple5 a1 a2 a3 a4 a5) = a3
type instance IndexedElement 4 (Tuple5 a1 a2 a3 a4 a5) = a4
type instance IndexedElement 5 (Tuple5 a1 a2 a3 a4 a5) = a5



newtype IndexedFamily i f = IndexedFamily (Map i f)
    deriving (Eq, Ord, Generic, Functor)

-- | Represents a term t indexed by i
newtype IndexedTerm i t = IndexedTerm (i -> t)
    deriving (Generic)
instance Newtype (IndexedTerm i t)

-- | Defines the lower and upper bounds for a sequence of 'IndexedTerm' values
newtype IndexRange i = IndexRange (i,i)
    deriving (Eq,Ord)

type instance Individual (IndexRange a) = a

type family MultiIndex (i::Nat) a = r | r -> i a where
    MultiIndex 1 a = UniTuple1 (IndexRange a)
    MultiIndex 2 a = UniTuple2 (IndexRange a)
    MultiIndex 3 a = UniTuple3 (IndexRange a)
    MultiIndex 4 a = UniTuple4 (IndexRange a)
    MultiIndex 5 a = UniTuple5 (IndexRange a)

-- | Characterizes a multi-level index    
class  (KnownNat i, Ord a) => MultiIndexed i a where
    -- | sets a multilevel index
    multix::UniTuple i (UniTuple 2 a) -> MultiIndex i a
    levels::MultiIndex i a -> SetSpec(IndexRange a)

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


ixrange::(a,a) -> IndexRange a
ixrange = IndexRange

term::(Integral i) => (i -> t) -> IndexedTerm i t
term = IndexedTerm

-- instance (OrdEnum a) => SetSpecBuilder (IndexRange a) where    
--     setspec (IndexRange (i,j)) = CountedSet count  s  where
--         s = [i..j]
--         count = add' (fromIntegral (List.length s)) 1

        
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

instance (OrdEnum a, Show a) => Show (IndexRange a) where
    show (IndexRange (a1,a2)) = "[" <> (show a1) <> "..." <> (show a2) <> "]"


-- instance (OrdEnum a) => MultiIndexed 1 a where    
--     multix (UniTuple1 r) =  UniTuple1 $ ixrange r
--     levels mix = setspec mix
        
-- instance (OrdEnum a) => MultiIndexed 2 a where    
--     multix (r1, r2) = (ixrange r1 , ixrange r2)
--     levels mix = setspec mix
                                    
-- instance (OrdEnum a) => MultiIndexed 3 a where    
--     multix (r1, r2, r3) = (ixrange r1, ixrange r2, ixrange r3)        
--     levels mix = setspec mix

-- instance (OrdEnum a) => MultiIndexed 4 a where    
--     multix (r1, r2, r3,r4) = (ixrange r1, ixrange r2, ixrange r3, ixrange r4)
--     levels mix = setspec mix

-- instance (OrdEnum a) => MultiIndexed 5 a where    
--     multix (r1, r2, r3, r4, r5) = (ixrange r1, ixrange r2, ixrange r3, ixrange r4, ixrange r5)
--     levels mix = setspec mix

instance (Eq a1) => NaturallyIndexed 1 (Tuple1 a1) where
    nix (Tuple1 a1) = a1
    
instance (Eq a1, Eq a2) =>  NaturallyIndexed 1 (Tuple2 a1 a2) where
    nix (a1,_) = a1
instance (Eq a1, Eq a2)  => NaturallyIndexed 2 (Tuple2 a1 a2) where
    nix (_,a2) = a2    

instance (Eq a1, Eq a2, Eq a3)  =>  NaturallyIndexed 1 (Tuple3 a1 a2 a3) where
    nix (a1,_,_) = a1
instance (Eq a1, Eq a2, Eq a3)  =>  NaturallyIndexed 2 (Tuple3 a1 a2 a3) where
    nix (_,a2,_) = a2        
instance (Eq a1, Eq a2, Eq a3)  =>  NaturallyIndexed 3 (Tuple3 a1 a2 a3) where
    nix (_,_,a3) = a3            

instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>  NaturallyIndexed 1 (Tuple4 a1 a2 a3 a4) where
    nix (a1,_,_,_) = a1
instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>   NaturallyIndexed 2 (Tuple4 a1 a2 a3 a4) where
    nix (_,a2,_,_) = a2        
instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>  NaturallyIndexed 3 (Tuple4 a1 a2 a3 a4) where
    nix (_,_,a3,_) = a3                
instance (Eq a1, Eq a2, Eq a3, Eq a4)  =>  NaturallyIndexed 4 (Tuple4 a1 a2 a3 a4) where
    nix (_,_,_,a4) = a4                    

instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5)  => NaturallyIndexed 1 (Tuple5 a1 a2 a3 a4 a5) where
    nix (a1,_,_,_,_) = a1
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NaturallyIndexed 2 (Tuple5 a1 a2 a3 a4 a5) where
    nix (_,a2,_,_,_) = a2        
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NaturallyIndexed 3 (Tuple5 a1 a2 a3 a4 a5) where
    nix (_,_,a3,_,_) = a3                
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NaturallyIndexed 4 (Tuple5 a1 a2 a3 a4 a5) where
    nix (_,_,_,a4,_) = a4                        
instance (Eq a1, Eq a2, Eq a3, Eq a4, Eq a5) => NaturallyIndexed 5 (Tuple5 a1 a2 a3 a4 a5) where
    nix (_,_,_,_,a5) = a5                            
                            