module Alpha.Canonical.Text.Conversion
(
    ToString(..),
    FromText(..),
    ToLines(..)
) where
import Alpha.Base
import Alpha.Canonical.Operators
import qualified Data.Text as Text


-- | Characterizes a value that can be converted to a 'String'
class ToString a where
    -- | Convers an 'a' value to a 'String'
    string::a -> String
                
-- | Characterizes a value that can be materialized from 'Text'
class FromText a where
    -- | Materializes an 'a'-value from text
    fromText::Text -> a

-- | Characterizes a value that can be converted to a list of 'Text' values
class ToLines a where
    -- | Converts an 'a' value to a list of 'Text' values
    lines::a -> [Text]    

instance ToString String where
    string = id
    
instance ToString Text where
    string x = Text.unpack x    

instance ToString Char where
    string x = [x]
    
instance ToLines Text where    
    lines = Text.lines

instance ToLines String where    
    lines = Text.lines . Text.pack