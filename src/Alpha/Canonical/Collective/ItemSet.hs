-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}

module Alpha.Canonical.Collective.ItemSet
(
    ItemSet(..),Setwise(..),IsSet(..)
) where
import Alpha.Base
import Alpha.Canonical.Functions
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary
import Alpha.Canonical.Relations
import Alpha.Canonical.Collective.Vacant
import Alpha.Canonical.Collective.Container
import Alpha.Canonical.Collective.Containers
import Alpha.Canonical.Collective.Appendable
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Semiring
import Alpha.Canonical.Algebra.Modular
import Alpha.Canonical.Algebra.Hetero
import Alpha.Canonical.Text
import Alpha.Canonical.Text.Asci


import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.Sequence as Sequence
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.MultiSet as Bag


type instance Element (ItemSet a) = a
type instance Appended (ItemSet (ItemSet a)) = ItemSet a        
type instance Summed (ItemSet a) (ItemSet a) = ItemSet a    



newtype ItemSet a = ItemSet (Set.Set a)
    deriving(Eq,Generic,Ord,Monoid,Semigroup,Foldable,Data,NFData,Finite,JoinSemiLattice,MeetSemiLattice,Lattice)
instance Newtype (ItemSet a)

-- | Characterizes a structure that can exhibit a list of elements
class Membership s where
    members::s -> ItemSet (Element s)         
instance Set (Membership a)    

-- | Characterizes types whose values can be treated as sets
class (Container c) => Setwise c where
    -- The union operator
    union::c -> c -> c
    -- The intersection operator
    intersect::c -> c -> c
    -- The set difference operator
    delta::c -> c -> c
    -- The set membership test operator
    subset::Bool -> c -> c -> Bool

class (Ord a) => IsSet a where
    set::a -> ItemSet (Item a)
        
instance (Ord a)  => IsSet [a] where    
    -- Constructs an 'ItemSet' from a list
    set::[a] -> ItemSet a
    set = fromList

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

instance (Ord a) => FiniteContainer (ItemSet a) where        

instance (Ord a) => Vacant (ItemSet a) where
    empty = ItemSet Set.empty
    null  = Set.null . unwrap

instance (Ord a) => Setwise (ItemSet a) where
    union x y = wrap $ Set.union (unwrap x) (unwrap y)
    intersect x y = wrap $ Set.intersection (unwrap x) (unwrap y) 
    delta x y = wrap $ Set.difference (unwrap x) (unwrap y) 
    subset proper candidate source 
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

instance Counted (ItemSet a) where
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
        
instance (Ord a, Unital a, Multiplicative a) => Semiring (ItemSet a)
    
instance  KnownNat n => Membership (Zn n) where    
    members s = residues s |> Set.fromList |> ItemSet

instance (Eq a, Ord a) => Setwise [a] where
    union = List.union
    intersect = List.intersect
    delta =  (List.\\)
    subset proper candidate source  
        = subset proper  (ItemSet $ Set.fromList candidate)  (ItemSet $ Set.fromList source)

    
instance (Ord a) => Setwise (Bag a) where
    intersect = Bag.intersection
    delta = Bag.difference
    union = Bag.union        
    subset proper candidate source 
        = ifelse proper 
            (Bag.isProperSubsetOf candidate source) 
            (Bag.isSubsetOf candidate source)

