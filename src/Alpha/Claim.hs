-----------------------------------------------------------------------------
-- | Operations that (in)validate claims
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}

module Alpha.Claim 
(
  Evaluation(..),
  Claim(..),
  success, failure, succeeded, failed, 
  claimSome, claimNone, claimEqual
)
where

import Alpha.Base


-- | Specifies the result of evaluating a claim
data Evaluation (a::Symbol) =
      Success
    | Failure
    deriving (Eq,Ord)


--show'::forall s. KnownSymbol s => Evaluation s -> String
    -- show' (Success) =  (symbolVal (Proxy @s)) ++ " -> Success"
    -- show' (Failure) =  (symbolVal (Proxy @s)) ++ " -> Failure"

instance KnownSymbol s => Show (Evaluation s) where
  show (Success) =  symbol @s <> " -> Success"
  show (Failure) =  symbol @s <> " -> Failure"


-- | Relates a symbol to a claim
class Claim (a::Symbol) where
  claim::Evaluation a
      
-- | Constructs an 'Evaluation' with a value of 'Success'             
success::KnownSymbol a => Evaluation a
success = Success

-- | Constructs an 'Evaluation' with a value of 'Failure'
failure::KnownSymbol a => Evaluation a
failure = Failure

-- | Claims that a 'Bool' value is true    
succeeded::KnownSymbol a => Bool -> Evaluation a
succeeded True = Success
succeeded False = Failure

-- | Claims that a 'Bool' value is false
failed::KnownSymbol a => Bool -> Evaluation a
failed True = Failure
failed False = Success

-- | Claims that 'Maybe' is valued
claimSome::KnownSymbol s => Maybe a -> Evaluation s
claimSome x = case isNothing x of
            False -> Success
            True -> Failure

-- | Claims that 'Maybe' is non-valued
claimNone::KnownSymbol s => Maybe a -> Evaluation s
claimNone x = case isNothing x of
            False -> Failure
            True -> Success

-- | Claims that two values are equal
claimEqual::(KnownSymbol s, Eq a) => a -> a -> Evaluation s
claimEqual x y = case (x == y) of
             True -> Success
             _ -> Failure

