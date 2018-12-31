-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Elementary.FiniteSet
(
    FiniteSet(..),
    Finite(..),
    finite,
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
    
-- | Represents a (discrete) finite set
newtype FiniteSet a = FiniteSet (Set' a)
    deriving (Eq, Generic, Ord, Monoid, Semigroup, Foldable, Data,
            NFData, JoinSemiLattice, MeetSemiLattice,Lattice,Typeable)

type instance Individual (FiniteSet a) = a

-- | Characterizes a type inhabited by a finite set of
-- values and for which a count is determined
class Finite a where
    -- | Counts the number of items within the purview of the subject
    count::(Integral n) => a -> n
    count a = individuals a |> List.length |> fromIntegral
    
    individuals::a -> [Individual a]

finite::(Ord a) => [a] -> FiniteSet a
finite s = FiniteSet (fromList s)

instance Newtype (FiniteSet a)
instance (Ord a) => Default (FiniteSet a) where
    def = FiniteSet []

instance (Ord a, Ord b) => Mappable (FiniteSet a) a b where
    type Mapped (FiniteSet a) a b = FiniteSet b
    map f s = wrap $ Set.map f (unwrap s)
    
instance (Ord a, Formattable a) => Formattable (FiniteSet a) where
    format (FiniteSet s) = list s |> embrace

instance (Ord a, Formattable a) => Show (FiniteSet a) where
    show = string . format
    
instance (Ord a) => IsList (FiniteSet a) where
    type Item (FiniteSet a) = a
    toList (FiniteSet s) = toList s
    fromList l = FiniteSet (fromList l)

instance Ord a => Finite (FiniteSet a) where
    count = fromIntegral . Set.size . unwrap
    individuals s = unwrap s |> Set.toList

instance Ord a => Vacant (FiniteSet a) where
    empty = FiniteSet Set.empty
    null  = Set.null . unwrap
    
instance (Ord a) => Unionizable (FiniteSet a) where
    union x y = wrap $ Set.union (unwrap x) (unwrap y)
    unions sets = (unwrap <$> sets) |> Set.unions |> FiniteSet 
    
instance Ord a => Intersectable (FiniteSet a) where
    intersect x y = wrap $ Set.intersection (unwrap x) (unwrap y) 

instance Ord a => Membership (FiniteSet a) where
    members (FiniteSet s) = s |> toList

instance Universal (FiniteSet a) where
    all pred s = unwrap s |> Set.toList |> List.all pred
            
instance Existential (FiniteSet a) where
    any pred s = unwrap s |> Set.toList |> List.any pred
    

instance Ord a => SetDifference (FiniteSet a) where
    diff x y = wrap $ Set.difference (unwrap x) (unwrap y) 

instance Ord a => SetContainment (FiniteSet a) where
    isSubset proper candidate source 
        = ifelse proper 
            (Set.isProperSubsetOf c' s') 
            (Set.isSubsetOf c' s') 
        where
            (c', s') = (unwrap candidate, unwrap source)
        
