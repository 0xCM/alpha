module Alpha.Canonical.Structures 
(
    Unital(..),
    Nullary(..),
    Invertible(..),
    Semigroup(..),
    Monoid(..),
    Group(..)
)
where


import Alpha.Canonical.Element

class (Membership s) => Unital s where
    one::Element s

class (Membership s) => Nullary s where
    zero::Element s
    
class (Membership s) => Invertible s where
    invert::Element s -> Element s    

class (Membership s) => Semigroup s where
    (<>)::Element s -> Element s -> Element s

class (Unital s, Semigroup s) => Monoid s where    
    mempty::Element s

class (Monoid s, Invertible s) => Group s