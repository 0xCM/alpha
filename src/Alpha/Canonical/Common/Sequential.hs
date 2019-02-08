-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Sequential
(
    module X,
    Listed(..),
    Bipartite(..),
    Paged(..),
    Sequential(..),
    BiReversible(..),
    Flippable(..),
    Reversible(..)

) where
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Individual as X
import Alpha.Canonical.Common.Synonyms as X
import Alpha.Canonical.Common.Types as X

import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Stream.Infinite as Stream
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import qualified Data.Text as Text

-- | Classifies a list-like structure
class Listed a where
    -- | Retrives the first item in the sequence
    head::a -> Individual a

    -- | Skips the first item of the sequence and returns the remainder
    tail::a -> a

    cons::Individual a -> a -> a

    snoc::a -> Individual a -> a

-- | Classifies a structure that can be partitioned into two sets
-- via a predicate
class Bipartite a where

    -- | Returns elements until a supplied predicate is disatisfied
    while::P1(Individual a) -> a -> a

    -- | Branches the source according to the outcome of a predicate:
    -- Elements that satisfy the predicate are branched right while the
    -- remainder are branched left
    split::P1(Individual a) -> a -> (a, a)
    
class Paged a  where 

    -- | Takes a n items from the front if they exist, othwise takes all
    take::(Integral n) => n -> a -> a


    splitAt::(Integral n) => n -> a -> (a, a)

    -- | Skips the first n elements and yields the remainder, if any
    skip::Integral n => n -> a -> a
    

class (Listed a, Bipartite a, Paged a) => Sequential a where

-- | Characterizes an aggregate where a notion of orientation prevails
-- on its constituents
class Reversible a where
    reverse::a -> a

class BiReversible a b | a -> b, b -> a  where
    bireverse::a -> b

class Flippable a where
    type Flipped a    
    flip::a -> Flipped a

instance (Ord a, Ord b) => Flippable (Map a b) where
    type Flipped (Map a b) = Map b a
    flip m = Map.toList m |> fmap (\(y,z) -> (z,y)) |> Map.fromList
    
instance Flippable (a -> b -> c) where
    type Flipped (a -> b -> c) = b -> a -> c
    flip = flip'

-------------------------------------------------------------------------------        
-- *Reversible instances
-------------------------------------------------------------------------------        
    
instance Reversible [a] where
    reverse = List.reverse
    
instance Reversible Text where    
    reverse = Text.reverse

-------------------------------------------------------------------------------        
-- *Listed instances
-------------------------------------------------------------------------------        
instance Listed (Vector a) where
    head = Vector.unsafeHead
    tail = Vector.unsafeTail
    cons = Vector.cons
    snoc = Vector.snoc
    
instance Listed [a] where
    head = List.head
    tail = List.tail  
    cons element list = element : list
    snoc list element = list  List.++ [element]

instance Listed (NonEmpty a) where        
    head = NonEmpty.head
    tail src = undefined
    cons = NonEmpty.cons
    snoc list element = undefined
    
instance Listed (Stream a) where        
    head s = s Stream.!! 0
    tail = Stream.tail
    cons element stream = Stream.prepend [element] stream
    snoc = undefined
        
instance Listed (Seq a) where    
    head s = Seq.index s 0
    tail s = snd $ Seq.splitAt 1 s
    cons = (Seq.<|)
    snoc = (Seq.|>)
        
-------------------------------------------------------------------------------        
-- *Bipartite instances
-------------------------------------------------------------------------------        
instance Bipartite [a] where
    while = List.takeWhile    
    split = List.partition        

instance Bipartite (Seq a)  where
    split = Seq.partition    
    while = Seq.takeWhileL 
    
-------------------------------------------------------------------------------        
-- *Paged instances
-------------------------------------------------------------------------------        
instance Paged [a] where
    take i src = fromList $ List.take (fromIntegral i) src
    splitAt = List.genericSplitAt
    skip n s = List.drop (fromIntegral n) s

instance  Paged (Seq a) where    
    take n s = Seq.take (fromIntegral n) s
    skip n s = Seq.drop (fromIntegral n) s
    splitAt i s = Seq.splitAt (fromIntegral i) s

-------------------------------------------------------------------------------        
-- *Sequential instances
-------------------------------------------------------------------------------            
instance Sequential [a]        
    
instance Sequential (Seq a)    

-------------------------------------------------------------------------------        
-- *Bireversible instances
-------------------------------------------------------------------------------            
instance BiReversible (a1,a2) (a2,a1) where
    bireverse (a1, a2) = (a2, a1)

instance BiReversible (a1, a2, a3) (a3, a2, a1) where
    bireverse (a1, a2, a3) = (a3, a2, a1)

instance BiReversible (a1, a2, a3, a4) (a4, a3, a2, a1) where
    bireverse (a1, a2, a3, a4) = (a4, a3, a2, a1)

instance BiReversible (a1, a2, a3, a4, a5) (a5, a4, a3, a2, a1) where
    bireverse (a1, a2, a3, a4, a5) = (a5, a4, a3, a2, a1)
    