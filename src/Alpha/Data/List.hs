-----------------------------------------------------------------------------
-- | List utilities
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Data.List
(    
    exclude,
    splitAt,
    mapi,
    intersperse,
    last,
    head,
    takeWhile,
    cycle,
    any,
    all,
    tails,    
    inits,
    reduce,
    scan,
    (List.++)
) where
import qualified Data.List as List
import Data.Functor
import Data.List(intersperse,last,head,takeWhile,cycle,partition,any,all)
import GHC.Real hiding(reduce)
import Prelude((-))
import Alpha.Base hiding(zero)
import Alpha.Canonical
import Alpha.Data.Numbers
import Alpha.Data.Seq


-- | Excludes a specified subset of list elements from the result list
exclude::(Eq a) => [a] -> [a] -> [a] 
exclude exclusions source = source |>  List.filter ( `List.notElem` exclusions) 

-- | Makes the 'List.genericSplitAt' function the 'default' splitAt function
splitAt::(Integral i) => i -> [a] -> ([a],[a])
splitAt = List.genericSplitAt

-- | Applies an indexed function to a list
mapi::(Integral i) => ((i, a)->b) -> [a] -> [b]
mapi f l = fmap f z where 
        idx = [zed..upper]
        upper  = sub' (length l) 1
        z = List.zip idx l

-- | Identical to List.tails except that the empty list is excluded from the result        
tails::[a] -> [[a]]
tails  = (List.filter (\x -> length x /= 0 )) . List.tails

-- | Identical to List.inits except that the empty list is excluded from the result
inits::[a] -> [[a]]
inits = (List.filter (\x -> length x /= 0 )) . List.inits

-- The reduction operator // takes a binary operator ⊕ on its left and a vector
-- x of values on its right. The meaning of ⊕//x for x = [a,b,...z] is the value a⊕b⊕...⊕z
-- See [Y1987TWOX]
reduce::(Nullary a) => (a -> a -> a) -> [a] -> a
reduce op (a:b:tail) =  op (op a b)  (reduce op tail)
reduce op (a:[]) = a
reduce op [] = zero

(//)::(Nullary a) => (a -> a -> a) -> [a] -> a
(//) = reduce
infixl 5 //

-- The scan operator \\ takes a binary operator ⊕ on its left and a vector
-- x ov values on the right and is defined by ⊕\\x for x = [a,b,...z] is the value [a,a⊕b,...,a⊕b..⊕z]
-- See [Y1987TWOX]
scan::(Nullary a) => (a -> a -> a) -> [a] -> [a]
scan op x = fmap (reduce op) (inits x)

(\\)::(Nullary a) => (a -> a -> a) -> [a] -> [a]
(\\) = scan
infixl 6 \\

instance Concatenable [a] [a] where
    type Concatenated [a] [a] = [a]
    concat x y = x List.++ y
        
instance (Eq a) => Sequential [a] a where
    listed = id
    take i src = fromList $ List.take (fromIntegral i) src
    split = List.partition
    tail = List.tail
    while = List.takeWhile
    filter = List.filter
    skip n s = List.drop (fromIntegral n) s

instance Length [a] where
    length x = List.length x |> convert

instance Collapsible [[a]] [a] where
    collapse = List.concat    

instance (Eq a) => Container [a] a where
    type Source [a] a = [a]    
    singleton x = [x]    

instance Reversible [a] [a] where
    reverse = List.reverse

instance Nullary [a] where
    zero = []

instance Degenerate [a] where
    degenerate a = length a == 0

instance Assembly [a] a where
    assemble = id
    disassemble = id
