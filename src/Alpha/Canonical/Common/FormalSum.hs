module Alpha.Canonical.Common.FormalSum
(
    FormalSum(..),
    formalsum,
) where
import Alpha.Canonical.Common.Root

-- Represents a formal sum
-- See https://en.wikipedia.org/wiki/Linear_combination
data FormalSum a b = FormalSum [[(a,b)]]

-- | Constructs a formal set of basis elements 
-- of type 'b' over a set of coefficients of type 'a'
formalsum::[b] -> [a] -> FormalSum a b
formalsum basis coeff = FormalSum $ do
    v <- basis
    return [(s, v) | s <- coeff]
