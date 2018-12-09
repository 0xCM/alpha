module Alpha.Canonical.Algebra.Pairing
(
    Paired(..), Pairing(..)

) where


type family Paired a b = r | r -> a b    

-- | Represents an ordered relationshp between two elements
class Pairing a b where

    -- | Pairs two elements
    pair::a -> b -> Paired a b

    --- | Extracts the first of the paired elements
    first::Paired a b -> a
    
    --- | Extracts the second of the paired elements
    second::Paired a b -> b

    swap::(Pairing b a) => Paired a b -> Paired b a
    swap x = pair (second x) (first x)
    