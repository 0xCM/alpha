{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Relations.Relation
(    
    Relation(..),
    Equivalence(..),
    Setoid(..),    
    PartialOrd(..),
    PartialOrder(..),
    JoinSemiLattice(..), 
    MeetSemiLattice(..), 
    Lattice(..),
    Membership(..),
    Convertible(..),
    Distinguished, distinguish,
    Related, relate
    

) where
import Algebra.PartialOrd 
import Alpha.Base
import Alpha.Canonical.Element
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations.Predicates
import Algebra.Lattice(JoinSemiLattice((\/)),MeetSemiLattice((/\)))
import Algebra.Lattice(Lattice(..))   

-- Characterizes a relation on a set s    
class Relation a where
    -- Relation adjudicator
    related::BinaryPredicate a

    -- Infix synonym for 'relator'
    (~~)::BinaryPredicate a
    (~~) = related
    infixl 6 ~~

-- Encodes that values a and by are related via a relation r
data Related r a b = Related r (a,b)
    deriving (Show,Ord,Eq)

    
-- Characterizes preorders that are symmetric, and hence 
-- define equivalence relations: a ~= b => b ~= a
class Relation a => Equivalence a where
    -- Equivalence relation adjudicator
    (~=)::BinaryPredicate a
    (~=) = (~~)

-- | A set together with an equivalence relation
-- See https://en.wikipedia.org/wiki/Setoid
class (Set a, Equivalence a) => Setoid a where

class (PartialOrd a, Relation a) =>  PartialOrder a where

    (~<=)::BinaryPredicate a
    (~<=) = leq
    infix 4 ~<=
    
-- | Codifies a (directed) conversion relationship between an input value and output value
class Convertible a b where
    -- | Requires that an 'a' value be converted to a 'b' value
    convert::a -> b    

newtype Distinguished a = Distinguished a
    deriving(Generic,Show)

instance Newtype (Distinguished a)

distinguish::a -> Distinguished a
distinguish = Distinguished

relate::r -> (a,b) -> Related r a b
relate  = Related