-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.List
(
    Reduced(..),
    exclude,
    Partitioner(..)

)
where

import Alpha.Canonical.Relations hiding(reduce)
import Alpha.Canonical.Collective.Container

import qualified Data.List as List
import qualified Data.Set as Set

type family Reduced a
type instance Reduced [a] = a
type instance Individual [a] = a

class Partitioner a where
    partition::Int -> [a] -> [[a]]
    partition width = List.takeWhile (not . List.null) . fmap (List.take width) . List.iterate (List.drop width)    

instance Partitioner a    


    

class Reductive a where
    -- The reduction operator // takes a binary operator ⊕ on its left and a vector
    -- x of values on its right. The meaning of ⊕//x for x = [a,b,...z] is the value a⊕b⊕...⊕z
    -- See [Y1987TWOX]
    reduce::O2 (Reduced a) -> a -> Reduced a

    (//)::O2 (Reduced a) -> a -> Reduced a
    (//) = reduce
    infixl 5 //

    -- The scan operator \\ takes a binary operator ⊕ on its left and a vector
    -- x ov values on the right and is defined by ⊕\\x for x = [a,b,...z] is the value [a,a⊕b,...,a⊕b..⊕z]
    -- See [Y1987TWOX]
    scan::O2 (Reduced a) -> a -> a    

    (\\)::O2 (Reduced a) -> a -> a
    (\\) = scan
    infixl 6 \\

-- | Identical to List.inits except that the empty list is excluded from the result
inits'::[a] -> [[a]]
inits' = (List.filter (\x -> List.length x /= 0 )) . List.inits

inits::(IsList a) => a -> [[Item a]]
inits x = fromList $ inits' $ toList x

-- | Excludes a specified subset of list elements from the result list
exclude::(Eq a) => [a] -> [a] -> [a] 
exclude exclusions source = source |>  List.filter ( `List.notElem` exclusions) 

instance (Eq a) => Container [a] where
    contain x = x
    contents = id

instance Concatenable [a] [a] where
    concat = (List.++)

instance Appendable [[a]] where
    append = List.concat    
    
instance (Eq a) => Filterable [a] where
    filter = List.filter
        
instance (Eq a, Ord a) => Setwise [a] where
    union = List.union
    intersect = List.intersect
    delta =  (List.\\)
    isSubset proper candidate source  
        = test (Set.fromList candidate) (Set.fromList source)
            where test = ifelse proper Set.isProperSubsetOf Set.isSubsetOf 
    
instance Vacant [a] where
    empty = []
    null = List.null
            
instance (Eq a) => Headed [a] where
    head = List.head
    tail = List.tail            

instance (Monoid a) => Reductive [a] where

    reduce::O2 a -> [a] -> a
    reduce op (a:b:tail) =  op (op a b)  (reduce op tail)
    reduce op (a:[]) = a
    reduce op [] = mempty

    scan::O2 a -> [a] -> [a]
    scan op x = (reduce op) <$> (inits x)                

instance (Eq a) => Sequential [a] where
    take i src = fromList $ List.take (fromIntegral i) src
    split = List.partition
    splitAt = List.genericSplitAt
    skip n s = List.drop (fromIntegral n) s
    while = List.takeWhile    
    
instance Mappable [a] a b where
    type Mapped [a] a b = [b]
    map = List.map
    
instance Groupable [a] where
    groups = List.groupBy

    
instance Zippable [a] [b] [c] where
    type LeftZip [a] [b] [c] = [a]
    type RightZip [a] [b] [c] = [b]
    type Zipped [a] [b] [c] = [c]    
    type Zipper [a] [b] [c] = a -> b -> c

    zip = List.zipWith