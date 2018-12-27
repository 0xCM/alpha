-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Elementary.Set
(
    module X,
    Set(..),
    SetBuilder(..),
    Subset(..),
    Discrete(..),
    DiscreteSubset(..),
    NonEmptySet(..),
    Constructible(..),
    subset',
    emptyset, finite, setrep, fneSet, discrep, indiscrete, neSet,

    isSetRep, isEmptySet, isFneSet, powerset
)
where
import Alpha.Canonical.Common
import Alpha.Canonical.Elementary.SetConstraints as X
import Alpha.Canonical.Elementary.FiniteSet as X
import Alpha.Canonical.Elementary.Individual as X
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Sequence as Sequence

        
-- | Classifies a type that can be interpreted as a set
-- where members can be distinquished from one another
-- via 'Eq'. Similar to a 'Setoid' where the equivalance
-- operation is equality.
--class Eq a => Set a where
data Set a 
    -- | Identifies a set representative of unspecified character
    = SetRep
    -- | Identifies a representative for a discrete and countable set
    | DiscreteRep
    -- | Identifies the empty set
    | EmptySet
    -- | Identifies a set that is non-discretizable and non-countable
    | IndiscreteRep   
    -- | Identifies a representative for which an explict set can be constructed
    | ConstructibleRep
    -- | Specifies a set with explicit content
    | UncountedSet [a]
     -- | Specifies a set with explicit finite content, if nonempty
    | CountedSet Int (FiniteSet a)
     -- | Specifies a set with explicit nonempty content
    | NeSet (NonEmpty a)
    -- | Specifies a finite non-empty set
    | FneSet (FiniteSet a)    
    deriving(Generic, Data, Typeable, Eq, Ord)

type instance Individual (Set a) = a

-- | Constructs a representative for the empty set relative to the parameter 'a'
emptyset::Set a
emptyset = EmptySet

-- | Constructs the default set representative
setrep::Set a
setrep = SetRep
    
-- | Constructs a finite, nonempty set
fneSet::(Ord a) => NonEmpty a -> Set a
fneSet (a :| items) = FneSet $ fromList (a:items)

-- | Constructs a representative for a discrete set
discrep::(Discrete a) => Set a
discrep = DiscreteRep

-- | Constructs a representative for a non-discretizable set
indiscrete::Set a
indiscrete = IndiscreteRep

-- | Constructs an explicit finite set if source is nonempty, otherwise 
-- returns the empty set
finite::(Ord a) => [a] -> Set a
finite items = 
    if (List.null items) 
        then EmptySet 
        else CountedSet 
            (List.length items) 
            (FiniteSet (fromList items))


countedset::(Ord a) => FiniteSet a -> Set a
countedset (FiniteSet s) = 
    if Set.null s 
        then EmptySet 
        else CountedSet (Set.size s) (FiniteSet s) 

neSet::(Ord a) => NonEmpty a -> Set a
neSet (a :| items) = NeSet $ fromList (a:items)

isFneSet::Set a -> Bool
isFneSet (FneSet _) = True
isFneSet _ = False


powerset::Ord a => Set a -> Set(Set a)
powerset (CountedSet _ []) = []
powerset (CountedSet n  (FiniteSet src)) = sets where    
    sets = Set.map FiniteSet (powerset' src) |> list |> (<$>) FneSet |> finite
powerset (FneSet (FiniteSet src)) = sets where
    sets =  Set.map FiniteSet (powerset' src) |> list |> (<$>) FneSet |> finite
            
isSetRep::Set a -> Bool
isSetRep (SetRep) = True
isSetRep (DiscreteRep) = True
isSetRep (EmptySet) = True
isSetRep (IndiscreteRep) = True
isSetRep _ = False

isEmptySet::Set a -> Bool
isEmptySet (EmptySet) = True
isEmptySet (UncountedSet l) = List.null l
isEmptySet (CountedSet n s) = n == 0
isEmptySet (FneSet s) = Set.null (unwrap s)
isEmptySet _ = False

subset'::forall a b. Subset a b => Set a
subset' = subset @a (SetRep @b)


class Constructible a where
    construct::Set a

-- | Characterizes a discrete set    
class Discrete a where
    individuals::Set a -> [Individual a]    

-- | Classifies a type whose values can be interpreted as a 
-- subset of another type's values
class Subset a b where
    subset::Set b -> Set a
    
-- | Classifies a type for which a discrete set of values can be interpreted as a 
-- subset of another type's values
class (Subset a b, Discrete a) => DiscreteSubset a b where
    subelements::Set b -> [Individual a]
    subelements b = individuals  (subset @a b)
    

class SetBuilder s where
    -- | Extracts the elements from a structure
    set::s -> Set (Individual s)

-- Defines a family of sets associated via an index    
newtype SetFamily i a = SetFamily (Map i (Set a))
    

instance Default (Set a) where
    def = EmptySet
    
instance Ord a => SetBuilder [a] where
    set items = 
        if (List.null items) 
        then EmptySet 
        else CountedSet count fs where
            count = (fromIntegral $ List.length items)
            fs = fromList items |> FiniteSet
    
instance Formattable TyConInfo where
    format (TyConInfo (_,mod,ctor)) = mod <> enclose LParen RParen ctor 

instance Show TyConInfo where
    show = string . format

instance forall a. (Formattable a, Typeable a, Ord a) => Formattable (Set a) where
    format x = fmt x
        where
            tyName = typerep @a |> show |> Text.pack            
            fmtU s = (List.take 10 s) |> (<$>) format |> List.intersperse Comma |> append |> enclose LBrace RBrace

            fmtC::Int -> [a] -> Text
            fmtC n s = format <$> s |> List.intersperse  Comma |> append |> enclose LBrace RBrace
        
            fmt (SetRep) =  rspaced tyName <> format (tycon @(SetRep::Set a)) 
            fmt (DiscreteRep) = rspaced tyName <> format (tycon @(DiscreteRep::Set a)) 
            fmt (EmptySet) = rspaced tyName <>  format (tycon @(EmptySet::Set a)) 
            fmt (IndiscreteRep) = rspaced tyName <> format (tycon @(IndiscreteRep::Set a)) 
            fmt (UncountedSet s) = fmtU s
            fmt (CountedSet n s) = fmtC n (list s)
            fmt (NeSet s) = (fmtU $ list s)
            fmt (FneSet (FiniteSet s)) = fmtC (length s) (list s)
                        
    
instance (Formattable a,Typeable a, Ord a) => Show (Set a) where
    show = Text.unpack . format 
        
instance Ord a => IsList (Set a) where
    type Item (Set a) = a
    toList (SetRep) = []
    toList (DiscreteRep) = []
    toList (EmptySet) = []
    toList (IndiscreteRep) = []
    toList (UncountedSet items) = toList items
    toList (CountedSet _ items) = toList items
    toList (NeSet items) = toList items
    toList (FneSet items) = toList items

    fromList [] = EmptySet
    fromList x = set x 

instance (Ord a) => Membership (Set a) where
    -- | Identifies a representative for which an explict set can be constructed
    --members (ConstructibleRep) =  members (construct::Set a)
    members (UncountedSet s) = s
    members (CountedSet _ (FiniteSet s)) = toList s
    members (NeSet s) = toList s
    members (FneSet s) = toList s  
    members(_) = []

instance (Ord a) =>  Unionizable (Set a) where
    union a b = set $ List.union (toList a) (toList b)    

    unions::[Set a] -> Set a
    unions sets = undefined --(unwrap <$> sets) |> Set.unions |> FiniteSet
    
instance Discrete Int8 where
    individuals _ = [minBound .. maxBound]
instance Discrete Int16 where
    individuals _ = [minBound .. maxBound]
instance Discrete Int32 where
    individuals _ = [minBound .. maxBound]
instance Discrete Int64 where
    individuals _ = [minBound .. maxBound]
instance Discrete Word8 where
    individuals _ = [minBound .. maxBound]
instance Discrete Word16 where
    individuals _ = [minBound .. maxBound]
instance Discrete Word32 where
    individuals _ = [minBound .. maxBound]
instance Discrete Word64 where
    individuals _ = [minBound .. maxBound]
instance Discrete Natural where
    individuals _ = [0..]


instance Constructible Int8 where
    construct = fneSet integers
instance Constructible Int16 where
    construct = fneSet integers
instance Constructible Int32 where
    construct = fneSet integers
instance Constructible Int64 where
    construct = fneSet integers
instance Constructible Word8 where
    construct = fneSet integers
instance Constructible Word16 where
    construct = fneSet integers
instance Constructible Word32 where
    construct = fneSet integers
instance Constructible Word64 where
    construct = fneSet integers
instance Constructible Natural where
    construct = fneSet (0 :| [1..])
        
instance Subset Int8 Int16 where
    subset _ = setrep 
instance Subset Int16 Int32 where
    subset _ = setrep 
instance Subset Int32 Int64 where
    subset _ = setrep 
instance Subset Int64 Integer where
    subset _ = setrep 
instance Subset Word8 Word16 where
    subset _ = setrep 
instance Subset Word16 Word32 where
    subset _ = setrep 
instance Subset Word32 Word64 where
    subset _ = setrep 
instance Subset Word64 Natural where
    subset _ = setrep 
instance Subset Natural Integer where
    subset _ = setrep 
                    
