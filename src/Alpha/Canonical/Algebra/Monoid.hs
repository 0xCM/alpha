module Alpha.Canonical.Algebra.Monoid
(
    MonoidalAlt, MonoidalProduct, MonoidalSum,
    altM, sumM, prodM

) where
import Alpha.Base    
import qualified Data.Monoid as Monoid

type MonoidalAlt f a = Monoid.Alt f a

type MonoidalSum a = Monoid.Sum a

type MonoidalProduct a = Monoid.Product a

-- Lifts the input into the Alt monoid
-- Example:
-- alt Nothing  <> alt (Just 4) <> alt (Just 7)
-- >> Alt {getAlt = Just 4}
altM::Monoid a => f a -> MonoidalAlt f a
altM = Monoid.Alt

sumM::Monoid a => a -> MonoidalSum a
sumM = Monoid.Sum

prodM::Monoid a => a -> MonoidalProduct a
prodM = Monoid.Product
    
