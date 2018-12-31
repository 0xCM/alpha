-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Collective.InfiniteSeq
(
    InfiniteSeq,
    seqI

) where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.Quantification
import Alpha.Canonical.Elementary.Individual
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Sequence as Sequence

-- | Represents a term in a sequence
newtype SeqTerm a = SeqTerm (Natural, a)
    deriving(Generic,Eq,Ord,Data,Typeable,Functor,Foldable,Traversable,Show,Formattable)
instance Newtype (SeqTerm a)

newtype InfiniteSeq a = InfiniteSeq [SeqTerm a]
    deriving (Eq, Generic, Ord, Monoid, Semigroup, Functor, Foldable, Traversable, Data, Typeable)
instance Newtype (InfiniteSeq a)

type instance Individual (InfiniteSeq a) = SeqTerm a

-- instance (Ord a, Ord b) => Mappable (InfiniteSeq a) a b where
--     type Mapped (InfiniteSeq a) a b = InfiniteSeq b
--     map f s = wrap $ List.map (\(i,a) -> (i, f a))  (unwrap s)

instance (Ord a) => IsList (InfiniteSeq a) where
    type Item (InfiniteSeq a) = SeqTerm a
    toList (InfiniteSeq s) = toList s
    fromList l = InfiniteSeq (fromList l)

instance (Ord a, Formattable a) => Formattable (InfiniteSeq a) where
    format (InfiniteSeq s) =  list s |> embrace

instance (Ord a, Formattable a) => Show (InfiniteSeq a) where
    show = string . format
    
instance Ord a => Membership (InfiniteSeq a) where
    members (InfiniteSeq s) = s
    
-- | Constructs an infinite sequence from via a supplied function 
-- defined on the domain of natural numbers   
seqI::(F1 Natural a) -> InfiniteSeq a
seqI f = InfiniteSeq [ SeqTerm (i, f i) | i <- [1..]]