-----------------------------------------------------------------------------
-- | List utilities
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Alpha.Data.List
(    
    exclude,
    split,
    exists,
    absent,
    replicate,
    splitAt,
    take,
    mapi,
    filter,    
    intersperse,
    last,
    head,
    takeWhile,
    cycle,
    partition,
    any,
    all,
    (List.++)
) where
import qualified Data.List as List
import Data.Functor
import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Numbers
import Data.List(filter,intersperse,last,head,takeWhile,cycle,partition,any,all,reverse)
import GHC.Real
import Prelude((-))


-- | Determines whether a distinguished element is absent from a list
-- absent::(Eq a) => a -> [a] -> Bool
-- absent = List.notElem


-- | Determines whether a distinguished element is present in a list
-- exists::(Eq a) => a -> [a] -> Bool
-- exists x l = List.notElem x l |> not

-- | Excludes a specified subset of list elements from the result list
exclude::(Eq a) => [a] -> [a] -> [a] 
exclude exclusions source = source |>  List.filter ( `absent` exclusions) 

-- | Partitions the list according to the outcome of a preducate
-- By convention, the elements that did not satisfy the predicate are
-- collected in the first coordinate of a tuple and those that did
-- accumulate in the second
split::(a ->Bool) -> [a] -> ([a],[a])
split predicate input =  
    (input |>List.filter predicate, input |> List.filter invert) 
        where invert = (\x -> not (predicate x))  

-- | Makes the 'List.genericTake' function the 'default' take function
take::(Integral i) => i -> [a] -> [a]
take = List.genericTake

-- | Makes the 'List.genericSplitAt' function the 'default' splitAt function
splitAt::(Integral i) => i -> [a] -> ([a],[a])
splitAt = List.genericSplitAt

-- | Makes the 'List.genericReplicate' function the 'default' replicate function
replicate::(Integral i) => i -> a -> [a]
replicate = List.genericReplicate


-- | Applies an indexed function to a list
mapi::(Integral i) => ((i, a)->b) -> [a] -> [b]
mapi f l = fmap f z where 
        idx = [zed..upper]
        upper  = sub' (length l) 1
        z = List.zip idx l

instance Concatenable [a] [a] where
    type Concatenated [a] [a] = [a]
    concat x y = x List.++ y
        
instance Enumerable [a] a where
    type Source [a] a = [a]
    items x = x

instance Length [a] where
    length x = List.length x |> convert

instance Reducible [[a]] where
    reduce = List.concat    

instance (Eq a) => Existential [a] a where
    exists x l = List.notElem x l |> not
        
instance Singletary [a] a where
    singleton x = [x]

instance Reversible [a] where
    reverse = List.reverse