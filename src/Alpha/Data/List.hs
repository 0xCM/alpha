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
    collapse,
    present,
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
    reverse,
    (List.++)
) where
import qualified Data.List as List
import Data.Functor
import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Numbers

import Data.List(filter,intersperse,last,head,takeWhile,cycle,partition,any,all,reverse)

instance Length [a] where
    length x = List.length x |> convert

instance Concatenable [a] [a] where
    type Concatenated [a] [a] = [a]
    concat x y = x List.++ y

instance Collapsable [[a]] where
    collapse = List.concat    

-- | Determines whether a distinguished element is absent from a list
absent::(Eq a) => a -> [a] -> Bool
absent = List.notElem

-- | Determines whether a distinguished element is present in a list
present::(Eq a) => a -> [a] -> Bool
present x l = List.notElem x l |> not

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
mapi::((Int, a)->b) -> [a] -> [b]
mapi f l = fmap f z where 
        idx = [0..(length l) - 1 ]
        z = List.zip idx l
