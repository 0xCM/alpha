-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Operators.Functions
(    
    Func, UnaryFunc, CartesianFunc, BinaryFunc, TernaryFunc,    

) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Element

-- A synonym for the canonical unary function type
type Func a b = a -> b

-- | Synonym for function that saturates with 1 argument
type UnaryFunc a b = Func a b

-- | Synonym for endomorphism
type EndoFunc a = Func a a

-- | Synonym for function that saturates with 1 cartesian argument
-- that aligns with the following definiion:
-- A **Cartesian function** is any function whose domain consists
-- of 2-tuples, i.e., f:(a,b) -> c 
type CartesianFunc a b c = Func (a,b) c

-- | Synonym for function that saturates with 2 heterogenous arguments
-- that aligns with the follwing definition
-- A **binary function** is a function that accepts two arguments and
-- produces a value. Note that a binary function cannot be cartesian
-- nor conversely.
type BinaryFunc a b c = a -> b -> c

-- | Function synonym that saturates with 3 heterogenous arguments
type TernaryFunc a b c d = a -> b -> c -> d
