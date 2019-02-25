-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Common.Morphisms
(
    module X,
    Morphic(..),
    Homomorphism(..),
    Endomorphism(..),
    Epimorphism(..),
    Monomorphism(..),
    Isomorphism(..),
) where    
import Alpha.Canonical.Common.Root as X


class Morphic (f::Type -> Type -> Type) s a b  where
    morphism::f a b -> (a -> b)
    
        
-- | Characterizes a structure-preserving map from one space to another
class Homomorphism a b where
    homomorph::a -> b

-- | Characterizes a structure-preserving map from a space to itself
class Homomorphism a a => Endomorphism a where
    endomorph::a -> a
    endomorph = homomorph

-- | Characterizes a surjective and structure-preserving map from one space to another
class Homomorphism a b => Epimorphism a b where
    epimorph::a -> b
    epimorph = homomorph
    
-- | Characterizes a surjective and structure-preserving map from one space to another
class Homomorphism a b => Monomorphism a b where
    monomorph::a -> b
    monomorph = homomorph

class (Monomorphism a b, Epimorphism a b) => Isomorphism a b where
    isomorph::a -> b
    isomorph = homomorph