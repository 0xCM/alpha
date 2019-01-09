-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Elementary.IndexedTerm
(
    module X,
    IndexedTerm(..), 
    IndexRange(..),    
    IndexedFamily(..),
    term,
    termix,
    ixrange,

) 
where
import Alpha.Canonical.Elementary.Common as X
import Alpha.Canonical.Elementary.Tuples as X
import Alpha.Canonical.Elementary.Set as X

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.Sequence as Sequence


-- | Represents a term t indexed by i
newtype IndexedTerm i t = IndexedTerm (i, (i -> t))
    deriving (Generic)
instance Newtype (IndexedTerm i t)

-- | Represents a family 'f' of elements with indexing set 'i'
newtype IndexedFamily i t = IndexedFamily (Map i (IndexedTerm i t))
    deriving (Eq, Ord, Generic, Functor)    
instance Newtype (IndexedFamily i t)    

-- | Defines the lower and upper bounds for an index
newtype IndexRange i = IndexRange (i,i)
    deriving (Eq,Ord,Generic,Functor)
instance Newtype (IndexRange i)    

type instance IndexedElement i (IndexedFamily i t) = IndexedTerm i t
type instance Individual (IndexRange i) = i

-- | Constructs an index range given a pair representing a lower/uppper bound    
ixrange::(OrdEnum a) => (a,a) -> IndexRange a
ixrange = IndexRange 

-- | Constructs a t-parametric term indexed by 'i'
term::i -> (i -> t) -> IndexedTerm i t
term i t = IndexedTerm (i,t)

-- | Determines the value of the terms's index
termix::IndexedTerm i t -> i
termix (IndexedTerm (i,t)) = i

ixfamily::(Ord i) => [IndexedTerm i t] -> IndexedFamily i t
ixfamily terms = (\t -> (termix t, t)) <$> terms |> Map.fromList |> IndexedFamily
        
instance (Ord k) => Indexed k (IndexedFamily k v) where
    at (IndexedFamily map) k = map Map.! k

instance OrdEnum i => Associated (IndexRange i) where
    associates (IndexRange (a,b)) = [a..b]
        
instance (Formattable i) => Formattable (IndexRange i) where
    format (IndexRange (min,max)) 
        = fence LBrack RBrack (format min <> spaced Dots <> format max)

instance (Formattable i) => Show (IndexRange i) where        
    show = string . format
    
instance (OrdEnum a) => SetBuilder (IndexRange a) a where    
    set (IndexRange (i,j)) = finset s  where
        s = [i..j]
        count = add' (fromIntegral (List.length s)) 1
        
instance (Formattable i, Formattable t) => Formattable (IndexedTerm i t) where
    format (IndexedTerm (i,f)) = format (i, f i)

instance (Formattable i, Formattable t) => Show (IndexedTerm i t) where
    show = string . format
    
instance Functor (IndexedTerm i) where
    fmap f (IndexedTerm (i,t)) = IndexedTerm (i, f . t) where
            
instance (Eq i) => Eq (IndexedTerm i t) where
    (IndexedTerm (i,t)) == (IndexedTerm (j,s)) = i == j

instance (Ord i) => Ord (IndexedTerm i t) where
    compare (IndexedTerm (i,t)) (IndexedTerm (j,s)) = compare i j
    
instance (Ord i) => IsList (IndexedFamily i a) where
    type Item (IndexedFamily i a) = IndexedTerm i a
    toList (IndexedFamily map) = associated map
    fromList l = ixfamily l
    