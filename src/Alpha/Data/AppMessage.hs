-----------------------------------------------------------------------------
-- | Operations and types related to application logging/messaging
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Alpha.Data.AppMessage
(
    Severity(..),
    AppMessage(..),
    babble, inform, warn, oops, doom
) where
import Alpha.Base


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
data AppMessage a = AppMessage Severity Text (Maybe a)
    deriving(Eq,Show)

construct::Severity -> Text -> Maybe a -> AppMessage a
construct sev msg payload = AppMessage sev msg payload

-- | Constructs a verbose message with an optional payload
babble::Text -> Maybe a -> AppMessage a
babble msg payload = construct Trivia msg payload

-- | Constructs an informative message with an optional payload
inform::Text -> Maybe a -> AppMessage a
inform msg payload = construct Info msg payload

-- | Constructs an warning message with an optional payload
warn::Text -> Maybe a -> AppMessage a
warn msg payload = construct Warn msg payload

-- | Constructs an error message with an optional payload
oops::Text -> Maybe a -> AppMessage a
oops msg payload = construct Error msg payload

-- | Constructs an fatal message with an optional payload
doom::Text -> Maybe a -> AppMessage a
doom msg payload = construct Fatal msg payload
