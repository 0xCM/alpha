-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Elementary.InfiniteSet
(
    InfiniteSet(..),
    infinite,
) where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.Quantification
import Alpha.Canonical.Elementary.Individual
import Alpha.Canonical.Elementary.SetConstraints
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Sequence as Sequence

-- | Represents a (discrete) infinite set
newtype InfiniteSet a = InfiniteSet [a]
    deriving (Eq, Generic, Ord, Monoid, Semigroup, Functor, Foldable, Traversable, Applicative, Monad, Data, Typeable)
instance Newtype (InfiniteSet a)

type instance Individual (InfiniteSet a) = a

infinite::(Ord a) => [a] -> InfiniteSet a
infinite = InfiniteSet

instance (Ord a, Ord b) => Mappable (InfiniteSet a) a b where
    type Mapped (InfiniteSet a) a b = InfiniteSet b
    map f s = wrap $ List.map f (unwrap s)

instance (Ord a) => IsList (InfiniteSet a) where
    type Item (InfiniteSet a) = a
    toList (InfiniteSet s) = toList s
    fromList l = InfiniteSet (fromList l)

instance (Ord a, Formattable a) => Formattable (InfiniteSet a) where
    format (InfiniteSet s) = (list s) |> List.take 10 |> embrace

instance (Ord a, Formattable a) => Show (InfiniteSet a) where
    show = string . format
    
instance (Ord a) => Unionizable (InfiniteSet a) where
    union x y = wrap $ List.union (unwrap x) (unwrap y)
    unions sets = undefined
        
instance Ord a => Membership (InfiniteSet a) where
    members (InfiniteSet s) = s
        