-----------------------------------------------------------------------------
-- | Structures over which sequential operations may be defined
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Sequential
(
    Headed(..),
    Sequential(..)
) where

import Alpha.Base
import Alpha.Canonical.Functions
import Alpha.Canonical.Elementary
import Alpha.Canonical.Collective.Headed

import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Stream.Infinite as Stream

import Prelude(fst,snd)



class (Headed a) => Sequential a  where 

    -- | Takes a n items from the front if they exist, othwise takes all
    take::(Integral n) => n -> a -> a

        -- | Returns elements until a supplied predicate is disatisfied
    while::P1 (Element a) -> a -> a
    
    -- | Branches the source according to the outcome of a predicate:
    -- Elements that satisfy the predicate are branched right while the
    -- remainder are branched left
    split::P1 (Element a) -> a -> (a, a)

    splitAt::(Integral n) => n -> a -> (a, a)

    -- | Skips the first n elements and yields the remainder, if any
    skip::Integral n => n -> a -> a


instance (Eq a) => Sequential [a] where
    take i src = fromList $ List.take (fromIntegral i) src
    split = List.partition
    splitAt = List.genericSplitAt
    skip n s = List.drop (fromIntegral n) s
    while = List.takeWhile    
    

instance (Eq a) => Sequential (Seq a) where    
    take n s = Seq.take (fromIntegral n) s
    split = Seq.partition    
    skip n s = Seq.drop (fromIntegral n) s
    splitAt i s = Seq.splitAt (fromIntegral i) s
    while = Seq.takeWhileL 
    

-- instance Sequential (Stream e) where    
--     take i s = Stream.take (fromIntegral i) s
--     split = Stream.partition
--     head s = s Stream.!! 0
--     while p s = undefined
--     tail = Stream.tail
--     skip i s = Stream.drop (fromIntegral i) s
--     splitAt i s = (take i s, skip i s)
    