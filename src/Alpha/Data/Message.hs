-----------------------------------------------------------------------------
-- | Operations and types related to application logging/messaging
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}

module Alpha.Data.Message
(
    Severity(..),
    Message(..),
    babble, inform, warn, oops, doom
) where
import Alpha.Base
import Alpha.Canonical
import Alpha.Data.Numbers


-- Defines a severity classifier
data Severity =
     Trivia -- Verbose
   | Info -- Neutral
   | Warn -- Warning
   | Error -- Error (recoverable)
   | Fatal -- Error (unrecoverable)
        deriving(Eq,Show,Enum,Ord,Data,Typeable)

instance Default Severity where
    def = Info        
        
-- Defines the structure of an application-level message
-- that is conditionally emitted predicated on a configurable
-- log level
data Message a = Message Severity Text (Maybe a)
    deriving(Eq,Show)

severity::Word -> Severity
severity val 
    = if | val == 0 -> Trivia  
         | val == 1 -> Info
         | val == 2 -> Warn
         | val == 3 -> Error
         | val >= 5 -> Fatal
      
construct::Severity -> Text -> Maybe a -> Message a
construct sev msg payload = Message sev msg payload

-- | Constructs a verbose message with an optional payload
babble::Text -> Maybe a -> Message a
babble msg payload = construct Trivia msg payload

-- | Constructs an informative message with an optional payload
inform::Text -> Maybe a -> Message a
inform msg payload = construct Info msg payload

-- | Constructs an warning message with an optional payload
warn::Text -> Maybe a -> Message a
warn msg payload = construct Warn msg payload

-- | Constructs an error message with an optional payload
oops::Text -> Maybe a -> Message a
oops msg payload = construct Error msg payload

-- | Constructs an fatal message with an optional payload
doom::Text -> Maybe a -> Message a
doom msg payload = construct Fatal msg payload
