-----------------------------------------------------------------------------
-- | Defines unified vocabulary for Text-related structures and operations
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Text 
(
    ToString(..),
    FromText(..),
    ToLines(..),
    Formattable(..),
    Packable(..),
    Faceted(..),
    text,

)
where
import Alpha.Base
import Alpha.Canonical.Algebra
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations

    
import qualified Data.Text as T
import qualified Data.String as S
import qualified Data.List as List
import qualified Prelude as S(lines)



text::(Show s) => s -> Text
text = T.pack . show

-- | Characterizes a pair of types for which transformations are defined 
-- for respectively "packing"  and "unpacking" type values
class Packable a b where
    -- Encodes an a-value to a b-value
    pack::a -> b
    -- Restores an a-value from a b-value
    unpack::b -> a    

-- | Characterizes a value that can be rendered in human-readable form
class Formattable a where
    format ::a -> Text

instance Formattable Text where
    format s = s
    
-- | Characterizes a value that can be converted to a 'String'
class ToString a where
    -- | Convers an 'a' value to a 'String'
    string::a -> String

instance ToString String where
    string = id

instance ToString T.Text where
    string x = T.unpack x    
                
-- | Characterizes a value that can be materialized from 'Text'
class FromText a where
    -- | Materializes an 'a'-value from text
    fromText::Text -> a

-- | Characterizes a value that can be converted to a list of 'Text' values
class ToLines a where
    -- | Converts an 'a' value to a list of 'Text' values
    lines::a -> [Text]    

class (KnownSymbol f) => Faceted f v where
    facetName::Text
    facetName =  symstr @f |> T.pack
    
instance ToLines T.Text where    
    lines = T.lines

instance ToLines String where    
    lines = T.lines .  T.pack 
        
instance Length T.Text where
    length t =   T.length t |> fromIntegral
        

