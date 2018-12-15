-----------------------------------------------------------------------------
-- | Abstractions inspired by list-like structure and operations
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Sequential
(
    Sequential(..)
) where

import Alpha.Base
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations.Predicates
import Alpha.Canonical.Element

import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Stream.Infinite as Stream

import Prelude(fst,snd)

class Sequential c  where 

    -- | Takes a n items from the front if they exist, othwise takes all
    take::(Integral n) => n -> c -> Seq (Element c)
    
    -- | Skips the first item of the sequence and returns the remainder
    tail::c -> c

    -- | Retrives the first item in the sequence
    head::c -> Element c

    -- | Branches the source according to the outcome of a predicate:
    -- Elements that satisfy the predicate are branched right while the
    -- remainder are branched left
    split::UnaryPredicate (Element c) -> c -> (c, c)

    -- | Returns elements until a supplied predicate is disatisfied
    while::UnaryPredicate (Element c) -> c -> c

    -- | Skips the first n elements and yields the remainder, if any
    skip::Integral n => n -> c -> c

instance (Eq a) => Sequential [a] where
    take i src = fromList $ List.take (fromIntegral i) src
    head = List.head
    split = List.partition
    tail = List.tail
    while = List.takeWhile    
    skip n s = List.drop (fromIntegral n) s
    
instance Sequential (Seq a) where    
    take n s =  Seq.take (fromIntegral n) s
    head s = Seq.index s 0
    tail s = snd $ Seq.splitAt 1 s
    while = Seq.takeWhileL 
    split = Seq.partition    
    skip n s = Seq.drop (fromIntegral n) s
    
instance Sequential (Stream e) where    
    take i s = fromList $ Stream.take (fromIntegral i) s
    split = Stream.partition
    head s = s Stream.!! 0
    while p s = undefined
    tail = Stream.tail
    skip n s = Stream.drop (fromIntegral n) s
    