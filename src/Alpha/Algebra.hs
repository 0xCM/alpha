module Alpha.Algebra
(
    Natural,
    NonEmpty,
    Additive, (+),
    Multiplicative, (*),
    Division, (/), (\\), (^),
    Factorable, factor,
    UnaryOp(..),
    BinaryOp(..),
    TernaryOp(..),
    (|>), (<|),
    
    Min(..), Max(..),
    First(..), Last(..),
    Endo(..),
    Dual(..),
    Sum(..),
    Product(..),

    Commutative(..),

    Invertible(..),

    Semigroup, (<>),
    Monoid, mempty, mappend, mconcat,
    Semigroupoid(..), compose, 
    Groupoid(..), 
    Group, (-), negate, subtract, times,    
    Rig,rig,
    
    LeftModule, (.*),
    RightModule,  (*.),
    Algebra, mult, 
    Coalgebra, comult,
    UnitalAlgebra, unit,
    Bialgebra(..),
    
    
    Field(..),    
    DivisionAlgebra, recipriocal,
    Order(..),    
    Ring(..),
    DivisionRing(..),
    Covector(..)

)
where
import GHC.Base(NonEmpty)
import Data.Semigroupoid
import Data.Groupoid(Groupoid(..))
import Data.Monoid(Dual(..), Endo(..), All(..), Monoid(..),  Product(..))
import qualified Data.Monoid as Monoid
import Data.Semigroup(Semigroup(..), Min(..), Max(..), First(..), Last(..), Sum(..), Product(..))
--import Data.Functor.Rep
import Data.Traversable
import Numeric.Algebra(Multiplicative, (*))
import Numeric.Algebra(Additive, (+))
import Numeric.Additive.Group(Group, (-), negate, subtract, times)
import Numeric.Algebra.Division(DivisionAlgebra, recipriocal,Division, (/), (\\), (^))
import Numeric.Algebra(Natural,Covector,Bialgebra,Factorable,Order,DivisionRing, Field, Ring)
import Numeric.Algebra(Algebra, mult)
import Numeric.Algebra(LeftModule, (.*), RightModule, (*.))
import Numeric.Algebra(Coalgebra, comult)
import Numeric.Algebra(Unital(..), UnitalAlgebra, unit)
import Numeric.Algebra.Commutative
import Numeric.Algebra.Factorable(Factorable,factorWith)
import Numeric.Algebra(Rig,fromNatural)
import Numeric.Algebra(Semiring(..))
import Numeric.Algebra(Monoidal,zero)
import qualified Numeric.Algebra as A

class A.Partitionable m => Partitionable m where
    partition::(m -> m -> r) -> m -> NonEmpty r
    partition f x = A.partitionWith f x 

-- | A constraint for anything that can be inverted as made precise by the law
-- flip . flip a = a. This is analagous to a group inversion operator but no
-- other context is required. 
class Invertible a where
    invert::a -> a

-- Alias for semigroupoid composition operator
compose::(Semigroupoid c) => c j k -> c i j -> c i k
compose = o

data Isomorphism k a b where
    Embedding:: k a b -> Isomorphism k a b
    Projection:: k a b -> Isomorphism k a b

rig::(Rig r) => Natural -> r
rig = fromNatural    


--`factor f c` returns a non-empty list containing `f a b` for all `a, b` such that `a * b = c`.
factor::(Factorable m) => (m -> m -> r) -> m -> NonEmpty r
factor f c = factorWith f c

-- Required signature for a unary operator
type UnaryOp a = a -> a

-- Required signature for a binary operator
type BinaryOp a = a -> a -> a    

-- Required signature for a ternary operator
type TernaryOp a = a -> a -> a -> a

-- | The forward pipe operator
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- | The backward pipe operator
infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = f x

