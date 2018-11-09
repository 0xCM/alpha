-----------------------------------------------------------------------------
-- | Defines the functorial API surface provided by external libraries
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Functor.Base
(
    Functor, fmap,(<$>), (<$), ($>),
    Apply, fapply, (<.>), (.>), (<.),
    AltF, (<!>),
    Bind, join, (>>-),(-<<), (-<-), (->-), 
    Plus, fzero,
    Foldable, foldr, fold,
    Traversable, traverse, feval,
    Bifoldable, bifoldl, bifoldr, bifold, bisum,
    Bitraversable, bitraverse,
    Bifunctor, bimap,
    Biapply(..),(<<$>>),(<<..>>),
    Extend, duplicated, extended,
    Const,
    Arrow, arr, 
    (***), (&&&), (^>>), (<<^), (>>^), (^<<), (>>>), (<<<),
    ArrowLoop, loop,
    Monad,
    Applicative, pure, (<*>), (<**>), liftA2, (<*), (*>),
    Alternative, empty, (<|>), optional,
    Comonad, extract, duplicate, extend, (=>=), (=<=),(<<=), (=>>),
    map
)
where
import Data.Functor.Bind(Bind(..),Apply(..), (<.>), (-<<), (-<-), (->-),join)
import Control.Category(Category, (.))
import Control.Arrow(Arrow, arr, (***), (&&&), (^>>), (<<^), (>>^), (^<<), (>>>), (<<<))
import Control.Arrow(ArrowLoop, loop)
import Control.Monad(Monad)
import Control.Applicative(Applicative, pure, (<*>), (<**>), liftA2, (<*), (*>))
import Control.Applicative(Alternative, empty, (<|>), optional)
import Control.Comonad(Comonad, extract, duplicate, extend, (=>=), (=<=),(<<=), (=>>)  )    
import Data.Traversable(Traversable, traverse, sequenceA)
import Data.Distributive(distribute, collect, cotraverse)
import Data.Foldable(Foldable,foldr, foldl, fold)
import Data.Functor.Alt(Alt, (<!>))
import Data.Functor.Plus(Plus)
import Data.Functor(Functor, fmap, (<$), ($>), (<$>))
import Data.Functor.Extend(Extend, duplicated, extended)
import Data.Functor.Const(Const)
import Data.Bifunctor.Sum(Sum(..))
import Data.Bifunctor.Product(Product(..))
import Data.Bifunctor(Bifunctor, bimap)
import Data.Bifunctor.Apply(Biapply(..), (<<$>>),(<<..>>))
import Data.Bifoldable
import Data.Bitraversable
import qualified Data.Functor.Plus as Plus

import Data.Int

type AltF = Alt

-- | Alias for Plus.zero as 'zero' is alreadly taken by 'Monoidal'
fzero::Plus f  => f a
fzero = Plus.zero

fapply::(Apply f) => (a->b->c) -> f a -> f b -> f c
fapply = liftF2
infixl 4 `fapply` 

feval::(Traversable t, Applicative f) => t (f a) -> f (t a)
feval = sequenceA

map::(Functor f) => (a -> b) -> f a -> f b
map = fmap
