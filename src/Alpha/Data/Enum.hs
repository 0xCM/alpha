-----------------------------------------------------------------------------
-- | Defines and re-exports common operators and combinators
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Data.Enum
(    
    
    firstValue, 
    lastValue,
    enumValues,
    Enum
        
)
where
import GHC.Enum
import qualified Data.List as List
import Alpha.Base
import Alpha.Canonical


-- | Retrieves the first value of an enum
firstValue::(Enum e) => e
firstValue = toEnum 0

-- | Retrieves the last value of a bounded enum
lastValue::(Bounded e, Enum e, Eq e) => e
lastValue = head (reverse enumValues)

-- | Retrieves all values of a bounded enum
enumValues::(Bounded e, Enum e) => [e]
enumValues = enumFrom firstValue

