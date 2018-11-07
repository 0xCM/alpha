-----------------------------------------------------------------------------
-- | Operations that (in)validate claims
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Claim where

import Alpha.Base

-- | Specifies the result of evaluating a claim
data Evaluation =
      Success
    | Failure

-- | Claims that two values are equal
equals::(Eq a) => a -> a -> Evaluation
equals x y = case (x == y) of
             True -> Success
             _ -> Failure

-- | Claims that a 'Bool' value is true    
true::Bool -> Evaluation
true True = Success
true False = Failure

-- | Claims that a 'Bool' value is false
false::Bool -> Evaluation
false True = Failure
false False = Success

-- | Claims that 'Maybe' is valued
some::Maybe a -> Evaluation
some x = case isNothing x of
            False -> Success
            True -> Failure

-- | Claims that 'Maybe' is non-valued
none::Maybe a -> Evaluation
none x = case isNothing x of
            False -> Failure
            True -> Success
