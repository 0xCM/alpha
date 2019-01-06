-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.List
(
    Reduced(..),
    Segmenter(..),
    exclude,
    nonempty,
    intermix,
    List.intersperse
)
where

import Alpha.Canonical.Relations hiding(reduce)
import Alpha.Canonical.Collective.Container

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NonEmpty

type family Reduced a
type instance Reduced [a] = a

type instance Individual [a] = a
type instance Individual (NonEmpty a) = a

class Segmenter a where
    segment::Int -> [a] -> [[a]]
    segment width = List.takeWhile (not . List.null) . fmap (List.take width) . List.iterate (List.drop width)    

instance Segmenter a    

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

-- | Creates a nonempty collection
nonempty::a -> [a] -> NonEmpty a
nonempty = (:|)

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


instance Appendable [[a]] where
    append = List.concat    
    
instance (Ord a) => Unionizable [a] where    
    union = List.union
    --unions lists = Set.fromList <$> lists |> Set.unions |> toList

instance (Eq a, Ord a) => Intersectable [a] where    
    intersect = List.intersect

instance (Eq a, Ord a) => Differential [a] where        
    diff =  (List.\\)
    
instance (Eq a, Ord a) => Containment [a] where        
    isSubset proper candidate source  
        = test (Set.fromList candidate) (Set.fromList source)
            where test = ifelse proper Set.isProperSubsetOf Set.isSubsetOf 
    
instance Vacuous [a] where
    empty = []
    null = List.null
            

instance (Monoid a) => Reductive [a] where

    reduce::O2 a -> [a] -> a
    reduce op (a:b:tail) =  op (op a b)  (reduce op tail)
    reduce op (a:[]) = a
    reduce op [] = mempty

    scan::O2 a -> [a] -> [a]
    scan op x = (reduce op) <$> (inits x)                

instance Headed [a] where
    head = List.head
    tail = List.tail            
    
instance Predicative [a] where
    while = List.takeWhile    
    split = List.partition
    
instance Paged [a] where
    take i src = fromList $ List.take (fromIntegral i) src
    splitAt = List.genericSplitAt
    skip n s = List.drop (fromIntegral n) s

instance Sequential [a]    

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


instance Container (NonEmpty a)

instance Length (NonEmpty a) where
    length = fromIntegral . NonEmpty.length

instance Mappable (NonEmpty a) a b where
    type Mapped (NonEmpty a) a b = NonEmpty b
    map = NonEmpty.map    
    
instance Weave a (NonEmpty a) where
    weave = NonEmpty.intersperse        

instance Headed (NonEmpty a) where        
    type Remaining (NonEmpty a) = [a]
    head = NonEmpty.head
    tail = NonEmpty.tail
        
instance Formattable a => Formattable (NonEmpty a) where
    format ne = format (toList ne)

intermix::[a] -> [a] -> [a]
intermix x y = join <| do
    i <- [0 .. n]
    [[x List.!! i, y List.!! i] ] where
        len = min (length x) (length y)  
        n = sub' len 1  
    


    



