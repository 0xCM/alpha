-----------------------------------------------------------------------------
-- | Defines the functorial API surface provided by external libraries
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Base.Functors
(
    Arrow(..), (^>>), (<<^), (>>^), (^<<), (>>>), (<<<),
    ArrowLoop(..),
    ArrowPlus, ArrowChoice, ArrowZero(..),
    Applicative(..), 
    (<**>), 
    Alternative, 
    Apply(..), fapply, 
    Alt(..), optional,
    Bind, join, (>>-),(-<<), (-<-), (->-), 

    Bifoldable(..), bisum,    
    Biapplicative(..), biliftA3,traverseBia, sequenceBia, traverseBiaWith,         
    Bitraversable(..),
    Bifunctor(bimap), 
    Biapply(..),(<<$>>),(<<..>>),
    
    Category, (.),
    Comonad(..), (=>=), (=<=),(<<=), (=>>),
    Const(..),
    Contravariant(..), phantom, ($<), (>$<),  (>$$<),
    Opposite, opposite,

    DistributiveF, fdistribute, cotraverse,
    Extend(..),
    Monad(..),
    Functor(..), (<$>), ($>),
    Foldable, foldMap, fold, foldr, foldr', foldl, foldl', foldby,
 
    Traversable(..), feval,

    ProductF(..), fprod,
    SumF(..), lsum, rsum,
    IdentityF(..), identity
    
)
where
import Control.Category
import Control.Arrow(Arrow(arr, (***), (&&&)))
import Control.Arrow(ArrowLoop(loop))
import Control.Arrow(ArrowPlus(..), ArrowChoice(..), ArrowZero(..))
import Control.Arrow((^>>), (<<^), (>>^), (^<<), (>>>), (<<<))
import Control.Monad(Monad(..))
import Control.Applicative(Applicative(pure, (<*>)),  (<**>), liftA2, (<*), (*>))
import Control.Applicative(Alternative(..), empty, (<|>))
import Control.Comonad(Comonad(..), extract, duplicate, extend, (=>=), (=<=),(<<=), (=>>))    
import Data.Traversable(Traversable(..), traverse, sequenceA)
import Data.Distributive(Distributive(distribute, collect), cotraverse)
import Data.Foldable(Foldable, foldMap, fold, foldr, foldr', foldl, foldl')
import Data.Functor.Bind(Bind(..),Apply((<.>), (<.), (.>),liftF2), (-<<), (-<-), (->-),join)
import Data.Functor.Alt(Alt((<!>),some,many), optional)
import Data.Functor(Functor(fmap,(<$)),(<$), ($>), (<$>))
import Data.Functor.Extend(Extend(duplicated, extended))
import Data.Functor.Const(Const(..))
import Data.Functor.Identity
import Data.Functor.Contravariant(Contravariant(..), Op(..), phantom, ($<), (>$<),  (>$$<))

import qualified Data.Functor.Product as Product
import qualified Data.Functor.Plus as Plus
import qualified Data.Functor.Sum as Sum


import qualified Data.Bifunctor.Sum as Bisum
import Data.Bifunctor(Bifunctor(..), bimap)
import Data.Bifunctor.Apply(Biapply(..), (<<$>>),(<<..>>))
import Data.Bifoldable(Bifoldable(..),bisum)
import Data.Bitraversable(Bitraversable(..))
import Data.Biapplicative(Biapplicative((<<*>>), bipure, biliftA2, (*>>), (<<*)))
import Data.Biapplicative(biliftA3,traverseBia, sequenceBia, traverseBiaWith)

import Data.Monoid(Monoid)

type DistributiveF f = Distributive f

-- A synonym for the Alt functor
type AltF = Alt

-- A synonym for the Product functor
type ProductF = Product.Product

-- A synonym for the Sum functor
type SumF = Sum.Sum

-- A synonym for the Identity functor
type IdentityF = Identity

-- A synonym for the Opposite (contravariant) arrow/function
type Opposite = Op

-- Constructs a product functor
fprod::(Functor f, Functor g) => f a -> g a -> ProductF f g a
fprod = Product.Pair

fdistribute::(DistributiveF g, Functor f) => f (g a) -> g (f a)
fdistribute = distribute

-- Constructs a left functorial sum
lsum::(Functor f, Functor g) => f a -> SumF f g a
lsum = Sum.InL

-- Constructs the identity functor
identity::(Functor f) => f a -> IdentityF (f a)
identity = Identity

-- Constructs a right functorial sum
rsum::(Functor f, Functor g) => g a -> SumF f g a
rsum = Sum.InR

fapply::(Apply f) => (a->b->c) -> f a -> f b -> f c
fapply = liftF2
infixl 4 `fapply` 

feval::(Traversable t, Applicative f) => t (f a) -> f (t a)
feval = sequenceA

opposite::(b -> a) -> Opposite a b
opposite f = Op {getOp = f}

-- | Folds a structure projected into a 'Monoid' by a supplied function
foldby :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldby = foldMap
