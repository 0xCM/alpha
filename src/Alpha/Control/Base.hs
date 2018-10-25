module Alpha.Control.Base
(
    Arrow, arr, 
    (***), (&&&), (^>>), (<<^), (>>^), (^<<), (>>>), (<<<),
    ArrowLoop, loop,
    Monad,
    Applicative, pure, (<*>), (<**>), liftA2, (<*), (*>),
    Alternative, empty, (<|>), optional,
    Comonad, extract, duplicate, extend, (=>=), (=<=),(<<=), (=>>)
)
where

import Control.Category
import Control.Monad
import Control.Arrow(Arrow, arr, (***), (&&&), (^>>), (<<^), (>>^), (^<<), (>>>), (<<<))
import Control.Arrow(ArrowLoop, loop)
import Control.Monad(Monad)
import Control.Applicative(Applicative, pure, (<*>), (<**>), liftA2, (<*), (*>))
import Control.Applicative(Alternative, empty, (<|>), optional)
import Control.Comonad(Comonad, extract, duplicate, extend, (=>=), (=<=),(<<=), (=>>)  )
    