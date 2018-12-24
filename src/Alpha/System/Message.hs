-----------------------------------------------------------------------------
-- | Operations and types related to application logging/messaging
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}

module Alpha.System.Message
(
    Severity(..),
    Message(..),
    babble, inform, warn, oops, doom,
    babble', inform', warn', oops', doom'
) where
import Alpha.Base
import Alpha.Canonical

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
      
define::Severity -> Text -> Maybe a -> Message a
define sev msg payload = Message sev msg payload

-- | Constructs a verbose message with an optional payload
babble'::Text -> Maybe a -> Message a
babble' msg payload = define Trivia msg payload

-- | Constructs a verbose message
babble::Text -> Message ()
babble msg = babble' msg none

-- | Constructs an informative message with an optional payload
inform'::Text -> Maybe a -> Message a
inform' msg payload = define Info msg payload

-- | Constructs an informative message
inform::Text -> Message ()
inform msg = inform' msg none

-- | Constructs an warning message with an optional payload
warn'::Text -> Maybe a -> Message a
warn' msg payload = define Warn msg payload

-- | Constructs an warning message
warn::Text ->  Message ()
warn msg = warn' msg none

-- | Constructs an error message with an optional payload
oops'::Text -> Maybe a -> Message a
oops' msg payload = define Error msg payload

-- | Constructs an error message
oops::Text -> Message ()
oops msg = oops' msg none

-- | Constructs an fatal message with an optional payload
doom'::Text -> Maybe a -> Message a
doom' msg payload = define Fatal msg payload

-- | Constructs an fatal message with an optional payload
doom::Text -> Message ()
doom msg = doom' msg none
