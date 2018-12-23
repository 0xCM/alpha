module Alpha.Canonical.Algebra.Orientation
(
    Sign(..),
    positive, negative, neutral, orientation
)
where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Additive

-- | Defines the codomain of the sign function
data Sign = Negative | Neutral | Positive
    deriving (Show,Ord,Eq,Enum)

            
-- Produces a 'Sign' of positive polarity
positive::Sign
positive = Positive

-- Produces a 'Sign' of negative polarity
negative::Sign
negative = Negative

-- Produces a 'Sign' of neutral polarity
neutral::Sign
neutral = Neutral

orientation::(Nullary a, Ord a) => a -> Sign
orientation a | a < zero  = Negative
        | a > zero = Positive
        | a == zero = Neutral
