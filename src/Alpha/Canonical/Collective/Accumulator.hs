-----------------------------------------------------------------------------
-- | Abstractions inspired by list-like structure and operations
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Accumulator
(
    Accumulator(..),
    tails, (List.++), 
   
)
where
import Alpha.Base
import qualified Data.List as List  

-- | Accumulation over a structure    
class Accumulator a where
    type Accumulation a

    accumulate::a -> Accumulation a    

-- | Identical to List.tails except that the empty list is excluded from the result        
tails::[a] -> [[a]]
tails  = (List.filter (\x -> List.length x /= 0 )) . List.tails
