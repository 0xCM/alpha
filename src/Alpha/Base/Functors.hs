-----------------------------------------------------------------------------
-- | Defines the functorial API surface provided by external libraries
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Base.Functors
(
    Arrow, arr, (***), (&&&), (^>>), (<<^), (>>^), (^<<), (>>>), (<<<),
    ArrowLoop, loop,
    ArrowPlus, ArrowChoice, ArrowZero,
    Applicative, pure, (<*>),  (<**>), liftA2, (<*), (*>),
    Alternative, (<|>),
    Apply, (<..>), (<.>), (.>), (<.), liftF2,  liftF3,
    Alt, optional,(<!>),some,many,
    Bind, (>>-), (-<<), (-<-), (->-),join,
    Bifunctor, bimap, (<<.>>), (.>>), (<<.),
    Bifoldable, bisum, bifoldMap,   
    Biapplicative, Bitraversable, (<<*>>), bipure, biliftA2, (*>>), (<<*), biliftA3, traverseBia, sequenceBia, traverseBiaWith, bitraverse,    
    Biapply,(<<$>>),(<<..>>), bilift2,bilift3,
    BindTrans, liftB,    
    Comonad, (=>=), (=<=),(<<=), (=>>),
    Const, fconst,
    Contravariant, phantom, ($<), (>$<),  (>$$<),
    OpF, fopposite,
    --Distributive, distribute, collect, cotraverse,
    Extend, duplicated, extended,
    Functor, fmap, (<$>), ($>), (<$),
    Functorial(..),
    Foldable, foldMap, fold, foldr, foldr', foldl, foldl', foldby, 
    --Identity, identity,
    Monad, (>>=),
    Traversable, traverse, sequenceA,
    --Product, product, -- Compatible with the notion of a categorical product
    --Sum, lsum, rsum, -- Compatible with the notion of a categorical coproduct    
    Eq1(..), Eq2(..), Ord1(..), Ord2(..), Show1(..), Show2(..), Read1(..),Read2(..),
)
where
import Control.Category
import Control.Arrow(Arrow, arr, (***), (&&&))
import Control.Arrow(ArrowLoop,loop)
import Control.Arrow(ArrowPlus, ArrowChoice, ArrowZero)
import Control.Arrow((^>>), (<<^), (>>^), (^<<), (>>>), (<<<))
import Control.Monad(Monad(..))
import Control.Applicative(Applicative, pure, (<*>),  (<**>), liftA2, (<*), (*>))
import Control.Applicative(Alternative, (<|>))
import Control.Comonad(Comonad, extract, duplicate, extend, (=>=), (=<=),(<<=), (=>>))    
import Data.Traversable(Traversable, traverse, sequenceA)
import Data.Distributive(Distributive, distribute, collect, cotraverse)
import Data.Foldable(Foldable, foldMap, fold, foldr, foldr', foldl, foldl')
import Data.Functor.Bind(Bind, (>>-), (-<<), (-<-), (->-),join)
import Data.Functor.Bind.Trans(BindTrans,liftB)
import Data.Functor.Alt(Alt((<!>),some,many), optional)
import Data.Functor.Apply(Apply, (<.>), (<.), (.>),liftF2, (<..>),liftF3)
import Data.Functor(Functor, fmap, (<$),(<$), ($>), (<$>))
import Data.Functor.Extend(Extend, duplicated, extended)
import Data.Functor.Const(Const(..))
import Data.Functor.Identity
import Data.Functor.Contravariant(Contravariant, Op(..), phantom, ($<), (>$<),  (>$$<))
import Data.Functor.Product(Product(..))
import Data.Functor.Sum 
import Data.Functor.Classes(Eq1(..),Ord1(..),Show1(..),Read1(..),Eq2(..),Ord2(..),Show2(..),Read2(..))
import Data.Bifunctor(Bifunctor, bimap)
import Data.Bifunctor.Apply(Biapply, (<<$>>),(<<..>>),bilift2,bilift3, (<<.>>), (<<.),(.>>))
import Data.Bifoldable(Bifoldable,bisum,bifoldMap)
import Data.Bitraversable(Bitraversable,bitraverse)
import Data.Biapplicative(Biapplicative, (<<*>>), bipure, biliftA2, (*>>), (<<*),biliftA3,traverseBia, sequenceBia, traverseBiaWith)
import Data.Monoid(Monoid)


-- Captures two functors which can be used to make a third via composition
newtype Functorial f g a = Functorial ( f (g a) )

instance (Functor f, Functor g) => Functor (Functorial f g) where
    fmap f (Functorial x) = Functorial (fmap (fmap f) x )

-- -- A synonym for the Sum functor
-- type SumF = Sum.Sum
fconst :: forall k a (b :: k). a -> Const a b
fconst = Const

-- A synonym for the Opposite (contravariant) arrow/function
type OpF = Op

-- Constructs a product functor
product::(Functor f, Functor g) => f a -> g a -> Product f g a
product = Pair


-- Constructs a left functorial sum
lsum::(Functor f, Functor g) => f a -> Sum f g a
lsum = InL

-- Constructs a right functorial sum
rsum::(Functor f, Functor g) => g a -> Sum f g a
rsum = InR

-- Constructs the identity functor
identity::(Functor f) => f a -> Identity (f a)
identity = Identity

--const::a -> b -> Const a b
--const = Const

-- | Constructs the opposite functor
fopposite::(b -> a) -> OpF a b
fopposite f = Op {getOp = f}

-- | Folds a structure projected into a 'Monoid' by a supplied function
foldby :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldby = foldMap
