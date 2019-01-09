-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Elementary.Common
(
    module X,
    Vacuous(..),
    Difference(..),
    Containment(..),
    Unionizable(..),
    Intersectable(..),    
    Singletary(..),
    Tabular(..),
    Transposable(..),
    IndexedElement(..),
    Indexed(..),
    NaturallyIndexed(..),
    SafelyIndexed(..),
    Queryable(..),

) where
import Alpha.Canonical.Common as X

import qualified Data.Map as Map
import qualified Numeric.Interval as Interval
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.MultiSet as Bag
import qualified Data.Set as Set
import qualified Data.Stream.Infinite as Stream
import qualified Data.Sequence as Sequence



-- | Characterizes a type that can be constructed from exactly one individual
class Singletary a where
    singleton::Individual a -> a

-- | Characterizes a rectangular data source
class Tabular a where
    rows::a -> [[Individual a]]
    cols::a -> [[Individual a]]
    
class Transposable a where
    type Transposed a
    transpose::a -> Transposed a
    
class Unionizable a where        
    -- The union operator
    union::a -> a -> a 

    unions::[a] -> a
    default unions::(Vacuous a, Unionizable a) => [a] -> a
    unions u = reduce empty union u
    
    
class Intersectable a  where
    intersect::a -> a -> a

    
class Difference a where
    diff::a -> a -> a

    (\\)::a -> a -> a
    (\\) = diff
    {-# INLINE (\\) #-}
    infix 5 \\
        
-- / Characterizes a type for which a canonical and unique Vacatable/void/empty
-- value exists
class Vacuous a where

    -- | Exhibits the canonical empty value
    empty::a

    -- | Determines whether a given value is the canonical
    -- 'empty' value
    null::a -> Bool

class Containment a where
    isSubset::Bool -> a -> a -> Bool

type family IndexedElement (i::k) a
type instance IndexedElement a (Map a b) = (a,b)
type instance IndexedElement Int (Vector a) = a
type instance IndexedElement Int (Seq a) = a
type instance IndexedElement Int [a] = a

-- | Characterizes a structure of type s holding elements indexed by a value of type i
class Indexed i a where

    at::a -> i -> IndexedElement i a

    (!)::a -> i -> IndexedElement i a
    (!) = at            
    infixr 9 !

class SafelyIndexed s i where
    
    lookup::s -> i -> Maybe (IndexedElement i s)

    (!?)::s -> i -> Maybe (IndexedElement i s)
    (!?) = lookup
    infixr 9 !?

-- | Characterizes an element indexed via type-level naturals    
class KnownNat i => NaturallyIndexed i a where
    natix::a -> IndexedElement i a

class Queryable a where

    -- | Excludes elements that don't satisfy a predicate
    filter::P1 (Individual a) -> a -> [Individual a]

    -- | Selects a single element, if any, that satisfies a predicate
    single::P1 (Individual a) -> a -> Maybe (Individual a) 
    single p src = ifelse (List.length filtered == 1) (Just $ List.head filtered) Nothing  
        where
            filtered = filter p src
                        
-- Vacuous instances
-------------------------------------------------------------------------------    
instance (Eq a) => Vacuous (Interval a) where
    empty = Interval.empty
    null = Interval.null

instance Vacuous [a] where
    empty = []
    null = List.null
    
instance Vacuous (Map k v) where
    empty = Map.empty
    null = Map.null
        
instance Vacuous (Bag a) where
    empty = Bag.empty
    null = Bag.null

instance Vacuous (Vector a) where
    empty = Vector.empty
    null = Vector.null
            
-- Transposable instances
-------------------------------------------------------------------------------        
instance Transposable [[a]] where
    type Transposed [[a]] = [[a]]
    transpose = List.transpose

-- Singletary instances
-------------------------------------------------------------------------------        

instance Ord k => Singletary (Map k v) where
    singleton (k,v) = fromList [(k,v)]
    
instance Singletary [a] where
    singleton a = [a]

instance Singletary (Seq a) where
    singleton x = fromList [x]

instance Singletary (Vector a) where
    singleton a = Vector.fromList [a]

instance Singletary (Stream a) where
    singleton x = Stream.cycle (x :| [])
        

instance (Ord a) => Singletary (Bag a) where
    singleton a = Bag.fromList [a]
    
-- Containment instances
-------------------------------------------------------------------------------            
instance (Ord a) => Containment (Bag a) where
    isSubset proper candidate source 
        = ifelse proper 
            (Bag.isProperSubsetOf candidate source) 
            (Bag.isSubsetOf candidate source)
    
instance (Eq a, Ord a) => Containment [a] where        
    isSubset proper candidate source  
        = test (Set.fromList candidate) (Set.fromList source)
            where test = ifelse proper Set.isProperSubsetOf Set.isSubsetOf 

instance Ord a => Containment (Set' a) where
    isSubset proper candidate source 
        = ifelse proper  
            (Set.isProperSubsetOf c' s')  
            (Set.isSubsetOf c' s')  
                where (c', s') = (candidate, source)
                        

-- Intersectable instances
-------------------------------------------------------------------------------            
instance (Ord a) => Intersectable (Bag a) where    
    intersect = Bag.intersection


instance (Eq a, Ord a) => Intersectable [a] where    
    intersect = List.intersect


-- Unionizable instances
-------------------------------------------------------------------------------            
    
instance (Ord a) => Unionizable [a] where    
    union = List.union
            
instance (Ord a) => Unionizable (Bag a) where    
    union = Bag.union        
            

-- Difference instances
-------------------------------------------------------------------------------            
instance (Ord a) => Difference (Bag a) where
    diff = Bag.difference

instance (Eq a, Ord a) => Difference [a] where        
    diff =  (List.\\)

instance (Ord a) => Difference (Set' a)  where        
    diff =  (Set.\\)
        
-- Indexed instances
-------------------------------------------------------------------------------            
instance (Eq a) => Indexed Int [a] where    
    at = (List.!!)

instance Indexed Int (Seq a) where
    at = Sequence.index
    
instance Indexed Int (Vector a) where
    at = (Vector.!)
    
instance (Ord k) => Indexed k (Map k v) where
    at map k = (k, map Map.! k)

instance (Ord k) => SafelyIndexed (Map k v) k where
     lookup map k = case (map Map.!? k)of
                        Just v -> Just (k, v)
                        _      -> Nothing

-- Queryable instances
-------------------------------------------------------------------------------            
instance Queryable [a] where
    filter pred source = List.filter pred source
    
instance Queryable (Seq a) where
    filter pred source =  toList source |> List.filter pred

