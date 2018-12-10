-----------------------------------------------------------------------------
-- | Defines morphism-related abstractions
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Morphic
(
    Morphism(..), Morphic(..),
    Hom(..), Dom(..), Cod(..),
    Compositional(..),
    Curried, Uncurried, curry, uncurry,
    type (~>)

) where
import Alpha.Base
import Data.Semigroupoid
import Alpha.Native
import Alpha.Canonical.Element
import qualified Data.Tuple as Tuple

-- Synonym for an 'uncurried' function
type Uncurried a b c = (a,b) -> c

-- Synonym for an 'curried' function
type Curried a b c =  a -> b -> c

type Hom a b = a -> b

-- Defines a function a -> b
data Morphism a b 
    = RuleMorphism (a -> b)    -- ^ | Specifies a morphism via a mapping rule
    | PairMorphism (Map a b)  -- ^ | Specifies a morphism via an explicit list of ordered pairs

type family (a ~> b)
type family Composition a c where
    Composition (Hom b c) (Hom a b) = Hom a c

--type Hom a b = a ~> b

type instance (a ~> b) = Hom a b

type family Dom f 
type instance Dom (Morphism a b) = a

type family Cod f     
type instance Cod (Morphism a b) = b


-- Characterizes function composition
class Compositional a b c where
    compose::Hom b c -> Hom a b -> Hom a c
    compose g f = g . f

-- Characterizes a function f
class Morphic f  where    
    -- The characterized function expressed via standard form
    morphism::f -> Morphism (Dom f) (Cod f)

    fx::f -> Dom f -> Cod f
    fx f x = undefined --(eval' morphism) x
    
        where
            eval'::Morphism (Dom f) (Cod f) -> Dom f -> Cod f
            eval' (RuleMorphism g) = undefined
            eval' (PairMorphism pairs) = undefined


instance Compositional (b,c) (a,b) (b,c)        


curry::Uncurried a b c -> Curried a b c
curry = Tuple.curry

uncurry::Curried a b c -> Uncurried a b c
uncurry = Tuple.uncurry

            