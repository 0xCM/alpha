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
import Alpha.Canonical.Elementary.Element
import Alpha.Canonical.Elementary.Elements
import Alpha.Canonical.Elementary.Tuples
import Alpha.Canonical.Elementary.Indexing
import Alpha.Canonical.Elementary.Discrete
import qualified Data.List as List

type family MultiIndex (i::Nat) a = r | r -> i a where
    MultiIndex 1 a = IndexRange a
    MultiIndex 2 a = UniTuple2 (IndexRange a)
    MultiIndex 3 a = UniTuple3 (IndexRange a)
    MultiIndex 4 a = UniTuple4 (IndexRange a)
    MultiIndex 5 a = UniTuple5 (IndexRange a)

-- | Characterizes a multi-level index    
class  (KnownNat i) => MultiIndexed i a where
    -- | Constructs a multilevel index
    multix::UniTuple i (UniTuple 2 a) -> MultiIndex i a
    levels::MultiIndex i a -> [IndexRange a]
        
-- | Represents a term t indexed by i
newtype IndexedTerm i t = IndexedTerm (i -> t)
    deriving (Generic)
instance Newtype (IndexedTerm i t)

-- | Defines the lower and upper bounds for a sequence of 'IndexedTerm' values
newtype IndexRange i = IndexRange (i,i)
    deriving (Eq,Ord)

    
term::(Integral i) => (i -> t) -> IndexedTerm i t
term = IndexedTerm

ixrange::(a,a) -> IndexRange a
ixrange = IndexRange

instance (OrdEnum a, Show a) => Show (IndexRange a) where
    show (IndexRange (a1,a2)) = "[" <> (show a1) <> "..." <> (show a2) <> "]"

instance (OrdEnum a) => Discrete (IndexRange a) where
    type Individual (IndexRange a) = a
    points (IndexRange (i,j)) = [i..j]

instance (OrdEnum a) => MultiIndexed 2 a where    
    multix (r1, r2) = (ixrange r1 , ixrange r2)
    levels mix = list mix
                                   
instance (OrdEnum a) => MultiIndexed 3 a where    
    multix (r1, r2, r3) = (ixrange r1, ixrange r2, ixrange r3)        
    levels mix = list mix

instance (OrdEnum a) => MultiIndexed 4 a where    
    multix (r1, r2, r3,r4) = (ixrange r1, ixrange r2, ixrange r3, ixrange r4)
    levels mix = list mix

instance (OrdEnum a) => MultiIndexed 5 a where    
    multix (r1, r2, r3, r4, r5) = (ixrange r1, ixrange r2, ixrange r3, ixrange r4, ixrange r5)
    levels mix = list mix

nest::(OrdEnum a) => IndexRange a -> IndexRange a -> [(a,a)]
nest r1 r2 = do
    a <- points r1
    b <- points r2
    return $ (a,b)
