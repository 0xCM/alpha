-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Collective.FiniteSeq
(
    FiniteSeq(..)

) where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.Individual
import Alpha.Canonical.Elementary.SetConstraints
import Alpha.Canonical.Elementary.Indexed
import Alpha.Canonical.Elementary.FiniteSet

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

-- | Represents a finite set of integrally-indexed terms    
newtype FiniteSeq i a = FiniteSeq (Map i a)
    deriving(Eq,Eq2,Ord,Ord2,Functor,Foldable,Traversable,Semigroup,Monoid,NFData,Generic,Data,Typeable)
instance Newtype(FiniteSeq i a)

type instance Individual (FiniteSeq i a) = (i,a)
type instance IndexedElement i (FiniteSeq i a) = Individual (FiniteSeq i a)

-- | Creates a finite sequence
fseq::(Integral i) => Seq (i,a) -> FiniteSeq i a
fseq terms = terms |> toList |> Map.fromList |> FiniteSeq

instance (Integral i) => Unionizable (FiniteSeq i a) where
    union x y = wrap $ Map.union (unwrap x) (unwrap y)
    unions sets = (unwrap <$> sets) |> Map.unions |> FiniteSeq

instance (Integral i) => IsList (FiniteSeq i a) where
    type Item (FiniteSeq i a) = (i,a)
    toList (FiniteSeq s) = toList s
    fromList l = FiniteSeq (fromList l)
    
instance (Integral i) => Indexed i (FiniteSeq i a) where
    at (FiniteSeq map) i = (i, map Map.! i)
    
instance Integral a => Finite (FiniteSeq i a) where
    count = fromIntegral . Map.size . unwrap
    individuals s = unwrap s |> Map.toList
    