-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Reduction
(
    Reduced(..),
    intermix,
)
where

import Alpha.Canonical.Relations hiding(reduce)

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NonEmpty

type family Reduced a
type instance Reduced [a] = a


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
                
instance (Monoid a) => Reductive [a] where

    reduce::O2 a -> [a] -> a
    reduce op (a:b:tail) =  op (op a b)  (reduce op tail)
    reduce op (a:[]) = a
    reduce op [] = mempty

    scan::O2 a -> [a] -> [a]
    scan op x = (reduce op) <$> (inits x)                

    
        
instance Formattable a => Formattable (NonEmpty a) where
    format ne = format (toList ne)

intermix::[a] -> [a] -> [a]
intermix x y = join <| do
    i <- [0 .. n]
    [[x List.!! i, y List.!! i] ] where
        len = min (length x) (length y)  
        n = sub' len 1  