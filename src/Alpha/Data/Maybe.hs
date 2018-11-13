module Alpha.Data.Maybe
(
    Maybe, just, isJust, none, isNone, fromJust
)
where

import Data.Bool
import Data.Maybe
import GHC.Base(($))

-- | Constructs a valued 'Maybe'
just :: a -> Maybe a
just x = Just x

-- | Constructs a non-valued 'Maybe'
none :: Maybe a
none = Nothing

-- | Determines whether the 'Maybe' is non-valued
isNone :: Maybe b -> Bool
isNone = isNothing


