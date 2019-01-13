-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Elementary.Sequential
(
    Listed(..),
    Bipartite(..),
    Paged(..),
    Sequential(..),

) where
import Alpha.Canonical.Common
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Stream.Infinite as Stream
import qualified Data.List.NonEmpty as NonEmpty

-- | Classifies a structure that can be partitioned into two sets:
-- A singleton set containing the "first" element and another set containing
-- the remainder
class Listed a where
    type Trailing a
    type Trailing a = a

    -- | Retrives the first item in the sequence
    head::a -> Individual a

    -- | Skips the first item of the sequence and returns the remainder
    tail::a -> Trailing a

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

instance Listed [a] where
    head = List.head
    tail = List.tail            

instance Bipartite [a] where
    while = List.takeWhile    
    split = List.partition        

instance Paged [a] where
    take i src = fromList $ List.take (fromIntegral i) src
    splitAt = List.genericSplitAt
    skip n s = List.drop (fromIntegral n) s
        
instance Sequential [a]        
    
instance Listed (NonEmpty a) where        
    type Trailing (NonEmpty a) = [a]
    head = NonEmpty.head
    tail = NonEmpty.tail
    
instance Listed (Stream a) where        
    head s = s Stream.!! 0
    tail = Stream.tail
        
instance Listed (Seq a) where    
    head s = Seq.index s 0
    tail s = snd $ Seq.splitAt 1 s
    
instance  Paged (Seq a) where    
    take n s = Seq.take (fromIntegral n) s
    skip n s = Seq.drop (fromIntegral n) s
    splitAt i s = Seq.splitAt (fromIntegral i) s

instance Bipartite (Seq a)  where
    split = Seq.partition    
    while = Seq.takeWhileL 

instance Sequential (Seq a)    


    