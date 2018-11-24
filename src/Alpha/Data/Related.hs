module Alpha.Data.Related
(
    Related, related
)
where

import Alpha.Canonical
import Alpha.Base

-- Encodes that values a and by are related via a relation r
data Related r a b = Related r (a,b)
    deriving (Show,Ord,Eq)

-- Defines a relation between a and b via r
related::r -> (a,b) -> Related r a b
related  = Related


