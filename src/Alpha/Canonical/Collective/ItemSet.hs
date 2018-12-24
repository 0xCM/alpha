-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}

module Alpha.Canonical.Collective.ItemSet
(
    ItemSet(..), IsSet(..)
) where
import Alpha.Canonical.Algebra
import Alpha.Canonical.Collective.Container
import Alpha.Canonical.Common.Asci

import qualified Data.List as List
import qualified Data.Sequence as Sequence
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.MultiSet as Bag


type instance Individual (ItemSet a) = a
type instance Appended (ItemSet (ItemSet a)) = ItemSet a        
type instance Summed (ItemSet a) (ItemSet a) = ItemSet a    


newtype ItemSet a = ItemSet (Set.Set a)
    deriving(Eq,Generic,Ord,Monoid,Semigroup,Foldable,Data,NFData,JoinSemiLattice,MeetSemiLattice,Lattice)
instance Newtype (ItemSet a)


class (Ord a) => IsSet a where
    iset::a -> ItemSet (Item a)
        
instance (Ord a)  => IsSet [a] where    
    -- Constructs an 'ItemSet' from a list
    iset::[a] -> ItemSet a
    iset = fromList

unions::(Ord a) => [ItemSet a] -> ItemSet a
unions sets = (unwrap <$> sets) |> Set.unions |> ItemSet

instance (Ord a) => IsList (ItemSet a) where
    type Item (ItemSet a) = a
    fromList = ItemSet .  Set.fromList
    toList   = Set.toList . unwrap

instance (Ord a) => Container (ItemSet a) where
    contain = fromList
    contents = toList

instance (Ord a) => Appendable (ItemSet (ItemSet a)) where
    append = unions . toList


instance (Ord a) => Vacant (ItemSet a) where
    empty = ItemSet Set.empty
    null  = Set.null . unwrap

instance (Ord a) => Setwise (ItemSet a) where
    union x y = wrap $ Set.union (unwrap x) (unwrap y)
    intersect x y = wrap $ Set.intersection (unwrap x) (unwrap y) 
    delta x y = wrap $ Set.difference (unwrap x) (unwrap y) 
    isSubset proper candidate source 
        = ifelse proper 
            (Set.isProperSubsetOf c' s') 
            (Set.isSubsetOf c' s') 
        where
            (c', s') = (unwrap candidate, unwrap source)
                
instance (Formattable a, Ord a) => Formattable (ItemSet a) where
    format x =  LBrace <> separated  <> RBrace where
        items = format <$> (toList x)
        separated = List.intersperse Comma items |> Text.concat
    
instance (Formattable a, Ord a) => Show (ItemSet a) where
    show = string . format

instance Finite (ItemSet a) where
    count = fromIntegral . Set.size . unwrap

instance (Ord a) => Filterable (ItemSet a) where
    filter p s = ItemSet $  Set.filter p  (unwrap s)
    
-- Algebraic aspects    
instance Pairing (ItemSet a) (ItemSet b) (DisjointUnion (ItemSet a) (ItemSet b)) where
    pair a b = DisjointUnion (a, b)
    first (DisjointUnion (a,b)) = a
    second (DisjointUnion (a,b)) = b

instance Universal (ItemSet a) where
    all pred s = unwrap s |> Set.toList |> List.all pred
        
instance Existential (ItemSet a) where
    any pred s = unwrap s |> Set.toList |> List.any pred

instance (Ord a, Unital a) =>  Unital (ItemSet a) where
    one = [one]
    
instance (Ord a, Multiplicative a) =>  Multiplicative (ItemSet a) where
    mul x y = intersect x y
    
instance (Ord a) =>  Additive (ItemSet a) where
    add x y = union x y
    {-# INLINE add #-}

instance (Ord a) => Nullary (ItemSet a) where
    zero = []

