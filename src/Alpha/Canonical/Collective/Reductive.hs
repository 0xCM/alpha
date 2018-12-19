-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Reductive
(

) where
import Alpha.Base
import Alpha.Canonical.Elementary
import Alpha.Canonical.Functions hiding(reduce)
import Alpha.Canonical.Algebra.Additive

import qualified Data.List as List

-- | Identical to List.inits except that the empty list is excluded from the result
inits'::[a] -> [[a]]
inits' = (List.filter (\x -> List.length x /= 0 )) . List.inits

inits::(IsList a) => a -> [[Item a]]
inits x = fromList $ inits' $ toList x


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
-- inits::[a] -> [[a]]
-- inits = (List.filter (\x -> List.length x /= 0 )) . List.inits

instance (Nullary a) => Reductive [a] where

    reduce::O2 a -> [a] -> a
    reduce op (a:b:tail) =  op (op a b)  (reduce op tail)
    reduce op (a:[]) = a
    reduce op [] = zero

    scan::O2 a -> [a] -> [a]
    scan op x = (reduce op) <$> (inits x)
