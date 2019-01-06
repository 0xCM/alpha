module Alpha.Canonical.Algebra.Orientation
(
    positive, negative, neutral, orientation
)
where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Additive


orientation::(Nullary a, Eq a, GT a, LT a) => a -> Sign
orientation a | a < zero  = Negative
        | a > zero = Positive
        | a == zero = Neutral
