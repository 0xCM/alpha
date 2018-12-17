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
  some, none, equal
)
where

import Alpha.Base hiding (none,some,equal)
import Alpha.Canonical.Text.Formattable
import qualified Data.Text as Text

-- | Specifies the result of evaluating a claim
data Evaluation (a::Symbol) =
      Success
    | Failure
    deriving (Eq,Ord)

instance KnownSymbol s => Formattable (Evaluation s) where
  format (Success) =  format (symbol @s) <> " -> Success"
  format (Failure) =  format (symbol @s) <> " -> Failure"
    

instance KnownSymbol s => Show (Evaluation s) where
  show = Text.unpack . format


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
some::KnownSymbol s => Maybe a -> Evaluation s
some x = case isNothing x of
            False -> Success
            True -> Failure

-- | Claims that 'Maybe' is non-valued
none::KnownSymbol s => Maybe a -> Evaluation s
none x = case isNothing x of
            False -> Failure
            True -> Success

-- | Claims that two values are equal
equal::(KnownSymbol s, Eq a) => a -> a -> Evaluation s
equal x y = case (x == y) of
             True -> Success
             _ -> Failure

