-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Elementary.Sequential
(
    Headed(..),
    Predicative(..),
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
class Headed a where
    type Remaining a
    type Remaining a = a

    -- | Retrives the first item in the sequence
    head::a -> Individual a

    -- | Skips the first item of the sequence and returns the remainder
    tail::a -> Remaining a
        
class Predicative a where
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
    
class (Headed a, Predicative a, Paged a) => Sequential a where

instance Headed [a] where
    head = List.head
    tail = List.tail            

instance Predicative [a] where
    while = List.takeWhile    
    split = List.partition        

instance Paged [a] where
    take i src = fromList $ List.take (fromIntegral i) src
    splitAt = List.genericSplitAt
    skip n s = List.drop (fromIntegral n) s
        
instance Sequential [a]        
    
instance Headed (NonEmpty a) where        
    type Remaining (NonEmpty a) = [a]
    head = NonEmpty.head
    tail = NonEmpty.tail
    
instance Headed (Stream a) where        
    head s = s Stream.!! 0
    tail = Stream.tail
        
instance Headed (Seq a) where    
    head s = Seq.index s 0
    tail s = snd $ Seq.splitAt 1 s
    
instance  Paged (Seq a) where    
    take n s = Seq.take (fromIntegral n) s
    skip n s = Seq.drop (fromIntegral n) s
    splitAt i s = Seq.splitAt (fromIntegral i) s

instance Predicative (Seq a)  where
    split = Seq.partition    
    while = Seq.takeWhileL 

instance Sequential (Seq a)    


    