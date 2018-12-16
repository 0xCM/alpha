module Alpha.Canonical.Structures 
(
    Structure(..),
    Unital(..),
    Nullary(..),
    Invertible(..),
    Semigroup(..),
    Monoid(..),
    Group(..)
)
where


import Alpha.Canonical.Element

class Structure a where
    elements::a -> [Element a]

instance Set (Structure a)


class (Structure a) => Multiplicative a where
    (*)::Element a -> Element a -> Element a

class (Structure a) => Unital a where
    one::Element a
    
class (Structure a) => Invertible a where
    invert::Element a -> Element a    
        
class (Structure a) => Additive a where
    (+)::Element a -> Element a -> Element a

class (Structure s) => Nullary s where
    zero::Element a
    
class (Structure a) => Negatable a where
    negate::Element a -> Element a    
        

class (Multiplicative s) => Semigroup s where
    
class (Semigroup a, Unital a) => Monoid a where    

class (Monoid s, Invertible s) => Group s where

class (Multiplicative a, Unital a, Additive a, Nullary a) => Semiring a