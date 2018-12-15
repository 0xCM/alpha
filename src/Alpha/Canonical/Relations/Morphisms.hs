-----------------------------------------------------------------------------
-- | Defines morphism-related vocabular
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Relations.Morphisms
(
    Morphism(..), 
    Hom(..), Dom(..), Cod(..),
    type (~>),
    pairMorphism,
    ruleMorphism,
    fx,

    
) where

import Alpha.Base
import Alpha.Canonical.Relations.Functions
import qualified Data.Map as Map
import Control.Category(Category(..))

instance Category PairFunc where

    id::forall a. PairFunc a a
    id = PairFunc Map.empty

    (.)::PairFunc b c -> PairFunc a b -> PairFunc a c
    (.) = undefined
    
-- Defines an element of Hom a b
data Morphism a b 
    = RuleMorphism (Func a b)      -- ^ | Specifies a morphism via a mapping rule
    | PairMorphism (PairFunc a b)  -- ^ | Specifies a morphism via an explicit list of ordered pairs


-- | Represents set of morphisms with domain Dom a and 
-- codomain Cod b
type Hom a b = Morphism a b

    
type family (a ~> b)

type instance Composition (Hom b c) (Hom a b) = Hom a c
    

type instance (a ~> b) = Hom a b

type family Dom f 
type instance Dom (Morphism a b) = a

type family Cod f     
type instance Cod (Morphism a b) = b

-- | Evaluates a morphism
fx::(Ord (Dom f)) => (Morphism (Dom f) (Cod f)) -> Dom f -> Cod f
fx (RuleMorphism g)  x =  g x
fx (PairMorphism (PairFunc pm)) x = pm Map.! x
{-# INLINE fx #-}

-- | Constructs a morphism predicated on a function defined via:
-- A **function** is a collection of ordered pairs, no two of which have the
-- same first term
pairMorphism::(Ord a) => [(a,b)] -> Morphism a b
pairMorphism = PairMorphism . PairFunc . fromList

-- | Constructs a morphism predicated on a function in the 'usual' FP sense
ruleMorphism::(a -> b) -> Morphism a b
ruleMorphism = RuleMorphism

