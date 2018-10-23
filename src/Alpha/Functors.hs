module Alpha.Functors
(
    Functor, fmap, (<$), ($>), (<$>),
    Alt, (<!>),
    Apply, apply, (<.>), (.>), (<.),
    Bind, join, (>>-),(-<<), (-<-), (->-), 
    Plus, fzero,
    Applicative, pure, (<*>), (<**>), liftA2, (<*), (*>),
    Alternative, empty, (<|>), optional,   
    Foldable, foldr, fold,
    Traversable, traverse, evaluate,
    Bifoldable, bifoldl, bifoldr, bifold, bisum,
    Bitraversable, bitraverse,
    Bifunctor, bimap,
    Biapply(..),(<<$>>),(<<..>>),
    Extend, duplicated, extended,
    Monad,
    Arrow, arr, (***), (&&&), (^>>), (<<^), (>>^), (^<<), (>>>), (<<<),
    ArrowLoop, loop
    -- Product(..),
    -- Sum(..)

)
where
import Control.Category
import Control.Arrow(Arrow, arr, (***), (&&&), (^>>), (<<^), (>>^), (^<<), (>>>), (<<<) )
import Control.Arrow(ArrowLoop, loop)
import Control.Monad(Monad)
import Control.Applicative(Applicative, pure, (<*>), (<**>), liftA2, (<*), (*>))
import Control.Applicative(Alternative, empty, (<|>), optional)

import Data.Traversable(Traversable, traverse, sequenceA)
import Data.Distributive(distribute, collect, cotraverse)

import Data.Foldable(Foldable,foldr, foldl, fold)

import qualified Data.Functor.Plus as Plus
import Data.Functor.Alt(Alt, (<!>))
import Data.Functor.Plus(Plus)
import Data.Functor.Bind(Bind(..),Apply(..),(-<<), (-<-), (->-))
import Data.Functor(Functor, fmap, (<$), ($>), (<$>))
import Data.Functor.Extend(Extend, duplicated, extended)
import Data.Bifunctor.Sum(Sum(..))
import Data.Bifunctor.Product(Product(..))
import Data.Bifunctor(Bifunctor, bimap)
import Data.Bifunctor.Apply(Biapply(..), (<<$>>),(<<..>>))
import Data.Bifoldable
import Data.Bitraversable

import Data.Int

-- | Alias for Plus.zero as 'zero' is alreadly taken by 'Monoidal'
fzero::Plus f  => f a
fzero = Plus.zero

apply::(Apply f) => (a->b->c) -> f a -> f b -> f c
apply = liftF2
infixl 4 `apply` 

evaluate::(Traversable t, Applicative f) => t (f a) -> f (t a)
evaluate = sequenceA