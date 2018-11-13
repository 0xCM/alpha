-----------------------------------------------------------------------------
-- | A pattern for sequential containers
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Data.Seq
(
    Seq(..),
    Sequential(..)
)
where

import Alpha.Base
import Alpha.Canonical
import Data.Sequence as S
import Prelude(fst,snd)

class (Container c e) => Sequential c e | c -> e where 
    
    -- | Returns the items as a list
    listed::Source c e -> [e]
    
    -- | Takes a n items from the front if they exist, othwise takes all
    take::(Integral n) => n -> Source c e -> Seq e
    
    -- | Skips the first item of the sequence and returns the remainder
    tail::Source c e -> Source c e

    -- | Branches the source according to the outcome of a predicate:
    -- Elements that satisfy the predicate are branched right while the
    -- remainder are branched left
    split::UnaryPredicate e -> Source c e -> (Source c e, Source c e)

    -- | Returns elements until a supplied predicate is disatisfied
    while::UnaryPredicate e -> Source c e -> Source c e

    -- | Excludes elements that don't satisfy a predicate
    filter::UnaryPredicate e  -> Source c e -> Source c e

    -- | Skips the first n elements and yields the remainder, if any
    skip::Integral n => n -> Source c e -> Source c e
    

instance Container (Seq a) a where
    type Source (Seq a) a = Seq a
    singleton = S.singleton


instance Sequential (Seq a) a where
    
    listed = toList
    take n s =  S.take (fromIntegral n) s
    tail s = snd $ S.splitAt 1 s
    while = S.takeWhileL 
    split = S.partition
    filter = S.filter
    skip n s = S.drop (fromIntegral n) s



    
