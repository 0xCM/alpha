module Alpha.Canonical.Structures 
(
    Multiplicative(..),
    Unital(..),
    Additive(..),
    Nullary(..),
    Semigroup(..),
    Monoid(..),
    Semiring(..),
)
where

import Alpha.Canonical.Algebra hiding (Group,Multiplicative,Unital,Nullary,Additive,Semiring,Monoid,Semigroup)
import qualified Alpha.Canonical.Algebra as A
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map

import Alpha.Canonical.Common.Asci


-- | Characterizes structures for which element multiplication is defined
class Multiplicative a where
    -- | Defines a structure's multiplication operator
    (*)::Individual a -> Individual a -> Individual a

class Multiplicative a => Semigroup a where        

class Semigroup a => Unital a where
    -- | Produces the multiplicative identity
    one::Individual a

class Unital a => Monoid a where    
    
class Additive a where
    -- | Defines a structure's addition operator
    (+)::Individual a -> Individual a -> Individual a

class Additive a => Nullary a where
    -- | Produces the additive identity
    zero::Individual a    

class Nullary a  => Negatable a where
    -- | Computes the additive inverse of an Individual
    negate::Individual a -> Individual a    


-- | Defines a semiring via the joined context
class (Unital a, Nullary a) => Semiring a where





