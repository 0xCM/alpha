{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- | Defines and re-exports common operators and combinators
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Alpha.Operators
(
    (/),
    ($),(<=), (>=),
    -- Bitwise operators
    firstValue, lastValue,
    left,right,
    module AM
    
)
where

import Control.Applicative(liftA2,Applicative)
import System.IO
import GHC.Enum
import GHC.Base(($),(<=), (>=))
import Text.Show
import Data.List(reverse, head, length, zip)
import Alpha.Data.Base
import Alpha.Data.Maybe as AM
import Alpha.Algebra


-- | Retrieves the first value of an enum
firstValue::(Enum e) => e
firstValue = toEnum 0

-- | Retrieves the last value of a bounded enum
lastValue::(Bounded e, Enum e) => e
lastValue = head (reverse values) 

-- | Retrieves all values of a bounded enum
values::(Bounded e, Enum e) => [e]
values = enumFrom firstValue

-- | Constructs a left-valued 'Either'
left :: l -> Either l r
left x = Left x

-- | Constructs a right-valued 'Either'
right :: r -> Either l r
right x = Right x

