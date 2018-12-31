module Alpha.Canonical.Structures 
(

)
where

import Alpha.Canonical.Algebra hiding (Group,Multiplicative,Unital,Nullary,Additive,Semiring,Monoid,Semigroup)
import qualified Alpha.Canonical.Algebra as A
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map

import Alpha.Canonical.Common.Asci


-- | Characterizes structures for which element multiplication is defined
class SMultiplicative a where
    -- | Defines a structure's multiplication operator
    smul::Individual a -> Individual a -> Individual a

class SMultiplicative a => SSemigroup a where        

class SSemigroup a => SUnital a where
    -- | Produces the multiplicative identity
    sone::Individual a

class SUnital a => SMonoid a where    
    
class SAdditive a where
    -- | Defines a structure's addition operator
    sadd::Individual a -> Individual a -> Individual a

class SAdditive a => SNullary a where
    -- | Produces the additive identity
    szero::Individual a    

class SNullary a  => SNegatable a where
    -- | Computes the additive inverse of an Individual
    snegate::Individual a -> Individual a    


-- | Defines a semiring via the joined context
class (SUnital a, SNullary a) => SSemiring a where



