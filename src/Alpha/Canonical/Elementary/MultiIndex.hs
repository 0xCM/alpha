-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Alpha.Canonical.Elementary.MultiIndex
(
    MultiIndex(..),
    MultiIndexed(..),
    IndexRange(..),
    IndexedTerm(..), 
    ixrange,
    term,
    nest
)
where
import Alpha.Base
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.Elements
import Alpha.Canonical.Elementary.Tuples
import Alpha.Canonical.Elementary.Indexing
import Alpha.Canonical.Elementary.Sets

import qualified Data.List as List


type instance Individual (IndexRange a) = a

-- | Defines the lower and upper bounds for a sequence of 'IndexedTerm' values
newtype IndexRange i = IndexRange (i,i)
    deriving (Eq,Ord)

type family MultiIndex (i::Nat) a = r | r -> i a where
    MultiIndex 1 a = UniTuple1 (IndexRange a)
    MultiIndex 2 a = UniTuple2 (IndexRange a)
    MultiIndex 3 a = UniTuple3 (IndexRange a)
    MultiIndex 4 a = UniTuple4 (IndexRange a)
    MultiIndex 5 a = UniTuple5 (IndexRange a)

-- | Characterizes a multi-level index    
class  (KnownNat i) => MultiIndexed i a where
    -- | sets a multilevel index
    multix::UniTuple i (UniTuple 2 a) -> MultiIndex i a
    levels::MultiIndex i a -> [IndexRange a]
        
-- | Represents a term t indexed by i
newtype IndexedTerm i t = IndexedTerm (i -> t)
    deriving (Generic)
instance Newtype (IndexedTerm i t)

term::(Integral i) => (i -> t) -> IndexedTerm i t
term = IndexedTerm

ixrange::(a,a) -> IndexRange a
ixrange = IndexRange

instance (OrdEnum a, Show a) => Show (IndexRange a) where
    show (IndexRange (a1,a2)) = "[" <> (show a1) <> "..." <> (show a2) <> "]"

instance (Eq a, OrdEnum a) => Set (IndexRange a)

instance (OrdEnum a) => SetBuilder (IndexRange a) a where    
    set (IndexRange (i,j)) = [i..j]

instance (OrdEnum a) => MultiIndexed 1 a where    
    multix (UniTuple1 r) =  UniTuple1 $ ixrange r
    levels mix = set mix
        
instance (OrdEnum a) => MultiIndexed 2 a where    
    multix (r1, r2) = (ixrange r1 , ixrange r2)
    levels mix = set mix
                                   
instance (OrdEnum a) => MultiIndexed 3 a where    
    multix (r1, r2, r3) = (ixrange r1, ixrange r2, ixrange r3)        
    levels mix = set mix

instance (OrdEnum a) => MultiIndexed 4 a where    
    multix (r1, r2, r3,r4) = (ixrange r1, ixrange r2, ixrange r3, ixrange r4)
    levels mix = set mix

instance (OrdEnum a) => MultiIndexed 5 a where    
    multix (r1, r2, r3, r4, r5) = (ixrange r1, ixrange r2, ixrange r3, ixrange r4, ixrange r5)
    levels mix = set mix

nest::(OrdEnum a) => IndexRange a -> IndexRange a -> [(a, a)]
nest r1 r2 = do
    a <- set r1
    b <- set r2
    return $ (a,b)
