-----------------------------------------------------------------------------
-- | Defines api surface of control-level constructs upon which alpha depends
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Base.Control
(
    Monad(..),
    filterM,
    forM,

    NFData(..), force, ($!!), ($!), deepseq,
    Category(..),
    Comonad(..),
    liftW,     -- :: Comonad w => (a -> b) -> w a -> w b
    wfix,      -- :: Comonad w => w (w a -> a) -> a
    cfix,     -- :: Comonad w => (w a -> a) -> w a
    kfix,     -- :: ComonadApply w => w (w a -> a) -> w a
    (=>=),
    (=<=),
    (<<=),
    (=>>),
    -- * Combining Comonads
    ComonadApply(..),
    (<@@>),    -- :: ComonadApply w => w a -> w (a -> b) -> w b
    liftW2,    -- :: ComonadApply w => (a -> b -> c) -> w a -> w b -> w c
    liftW3,    -- :: ComonadApply w => (a -> b -> c -> d) -> w a -> w b -> w c -> w d
    -- * Cokleisli Arrows
    Cokleisli(..),
    -- * Functors
    Functor(..),
    (<$>),     -- :: Functor f => (a -> b) -> f a -> f b
    ($>),      -- :: Functor f => f a -> b -> f b
    
    bracket,

)
where
import Control.Category
import Control.Monad
import Control.Comonad
import Control.DeepSeq
import Control.Exception(bracket)
import GHC.Base(($!))
