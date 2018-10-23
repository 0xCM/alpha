module Alpha.Data.Maybe
(
    some, isSome, none, isNone, fromJust
)
where

import Data.Bool
import Data.Maybe
import GHC.Base(($))

-- | Constructs a valued 'Maybe'
some :: a -> Maybe a
some x = Just x

-- | Determines whether the 'Maybe' is valued
isSome :: Maybe a -> Bool
isSome x =  not $ isNothing x  

-- | Constructs a non-valued 'Maybe'
none :: Maybe a
none = Nothing

-- | Determines whether the 'Maybe' is non-valued
isNone :: Maybe b -> Bool
isNone = isNothing


