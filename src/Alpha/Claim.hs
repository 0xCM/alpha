-----------------------------------------------------------------------------
-- | Operations that (in)validate claims
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}

module Alpha.Claim 
(
  Outcome(..),
  Claim(..),
  Example(..),
  success, failure, succeeded, failed, 
  some, none, equal
)
where

--import Alpha.Base hiding (none,some,equal)
import Alpha.Canonical.Common hiding (none,some,equal)
import qualified Data.Text as Text

-- | Defines an example identified by a symbol
class (KnownSymbol s) => Example s  where
  type Exemplar s
  type Exemplar s = IO()
  
  example::Exemplar s
  
  name::String
  name = symstr @s 


-- | Specifies the result of evaluating a claim
data Outcome a =
      Success
    | Failure
    deriving (Eq,Ord)


-- | Defines a claim identified by a symbol
class (KnownSymbol s) => Claim s where
  claim::Outcome s

instance KnownSymbol s => Formattable (Outcome s) where
  format (Success) =  format (symbol @s) <> " -> Success"
  format (Failure) =  format (symbol @s) <> " -> Failure"
      
  
instance KnownSymbol s => Show (Outcome s) where
  show = Text.unpack . format    

-- | Constructs an 'Outcome' with a value of 'Success'             
success::KnownSymbol a => Outcome a
success = Success

-- | Constructs an 'Outcome' with a value of 'Failure'
failure::KnownSymbol a => Outcome a
failure = Failure

-- | Claims that a 'Bool' value is true    
succeeded::KnownSymbol a => Bool -> Outcome a
succeeded True = Success
succeeded False = Failure

-- | Claims that a 'Bool' value is false
failed::KnownSymbol a => Bool -> Outcome a
failed True = Failure
failed False = Success

-- | Claims that 'Maybe' is valued
some::KnownSymbol s => Maybe a -> Outcome s
some x = case isNothing x of
            False -> Success
            True -> Failure

-- | Claims that 'Maybe' is non-valued
none::KnownSymbol s => Maybe a -> Outcome s
none x = case isNothing x of
            False -> Failure
            True -> Success

-- | Claims that two values are equal
equal::(KnownSymbol s, Eq a) => a -> a -> Outcome s
equal x y = case (x == y) of
             True -> Success
             _ -> Failure