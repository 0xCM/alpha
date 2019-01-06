-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Elementary.MultiIndex
(
    MultiIndex(..),
    MultiIndexed(..),
    testIndex,
) where
import Alpha.Canonical.Elementary.Common
import Alpha.Canonical.Elementary.Indexed
import Alpha.Canonical.Elementary.Tuples
import Alpha.Canonical.Elementary.IndexedTerm

type family MultiIndex (i::Nat) a = r | r -> i a where
    MultiIndex 1 a = UniTuple1 (IndexRange a)
    MultiIndex 2 a = UniTuple2 (IndexRange a)
    MultiIndex 3 a = UniTuple3 (IndexRange a)
    MultiIndex 4 a = UniTuple4 (IndexRange a)
    MultiIndex 5 a = UniTuple5 (IndexRange a)

-- | Characterizes a multi-level index    
class  (KnownNat i, OrdEnum a) => MultiIndexed i a where
    multiIndex::UniTuple i (IndexRange a) -> MultiIndex i a
    multiLevels::MultiIndex i a -> [UniTuple i a]


instance (OrdEnum a) => MultiIndexed 1 a where    
    multiIndex (UniTuple1 r) =  UniTuple1 r
    multiLevels (UniTuple1 r) = [UniTuple1 a | a <- associates r]

instance (OrdEnum a) => MultiIndexed 2 a where    
    multiIndex (r1, r2) = (r1, r2)
    multiLevels (r1, r2) 
        = [(a1,a2) | a1 <- associates r1, a2 <- associates r2]
                    
instance (OrdEnum a) => MultiIndexed 3 a where    
    multiIndex (r1, r2, r3) = (r1 , r2, r3)
    multiLevels (r1, r2, r3) 
        = [(a1,a2,a3) | a1 <- associates r1, a2 <- associates r2, a3 <- associates r3]

instance (OrdEnum a) => MultiIndexed 4 a where    
    multiIndex (r1, r2, r3, r4) = (r1 , r2, r3, r4)
    multiLevels (r1, r2, r3, r4) 
        =  [(a1,a2,a3,a4) | 
                a1 <- associates r1, 
                a2 <- associates r2, 
                a3 <- associates r3, 
                a4 <- associates r4]

instance (OrdEnum a) => MultiIndexed 5 a where    
    multiIndex (r1, r2, r3, r4, r5) = (r1 , r2, r3, r4, r5)
    multiLevels (r1, r2, r3, r4, r5) 
        =  [(a1,a2,a3,a4,a5) | 
                a1 <- associates r1, 
                a2 <- associates r2, 
                a3 <- associates r3, 
                a4 <- associates r4, 
                a5 <- associates r5]
    
testIndex = mi where
    r1 = ixrange (1,10)
    r2 = ixrange (20,25)
    r3 = ixrange (50,60)
    mi = multiIndex @3 (r1,r2,r3)
                