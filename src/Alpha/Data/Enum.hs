{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- | Defines and re-exports common operators and combinators
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Alpha.Data.Enum
(    
    -- Bitwise operators
    firstValue, 
    lastValue,
    values
        
)
where
import GHC.Enum
import Data.List(reverse, head, length, zip)
import Alpha.Canonical

-- | Retrieves the first value of an enum
firstValue::(Enum e) => e
firstValue = toEnum 0

-- | Retrieves the last value of a bounded enum
lastValue::(Bounded e, Enum e) => e
lastValue = head (reverse values) 

-- | Retrieves all values of a bounded enum
values::(Bounded e, Enum e) => [e]
values = enumFrom firstValue

