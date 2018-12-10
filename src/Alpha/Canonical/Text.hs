-----------------------------------------------------------------------------
-- | Defines unified vocabulary for Text-related structures and operations
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Text 
(
    module Asci,
    ToString(..),
    FromText(..),
    ToLines(..),
    Formattable(..),
    Packable(..),
    Faceted(..),
    text,
    enclose,
    parenthetical, 
    embrace,
    spaced,
    suffix,
    prefix,
)
where
import Alpha.Base
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations
import Alpha.Canonical.Text.Asci as Asci


import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.String as S
import qualified Data.List as List
import qualified Prelude as S(lines)

text::(Show s) => s -> Text
text = Text.pack . show

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

class (KnownSymbol f) => Faceted f v where
    facetName::Text
    facetName =  symstr @f |> pack
    
enclose::(Formattable l, Formattable c, Formattable r) => l -> r -> c -> Text
enclose left right content = Text.concat [format left, format content, format right]

-- | Fences content between left and right braces
embrace::(Formattable a) => a -> Text
embrace content = enclose LBrace RBrace content 

-- | Fences content between left and right parenthesis
parenthetical::(Formattable a) => a -> Text
parenthetical content = enclose LParen RParen content 

-- | Surrounds the input text within a space on each side
spaced::Text -> Text
spaced t = Space <> t <> Space

suffix::Text -> Text -> Text
suffix = Text.append

prefix::Text -> Text -> Text
prefix a b = Text.append b a

instance Packable String Text where
    pack = Text.pack
    unpack = Text.unpack

instance ToString String where
    string = id
    
instance ToString Text where
    string x = unpack x    

instance ToString Char where
    string x = [x]
    
instance ToLines Text where    
    lines = Text.lines

instance ToLines String where    
    lines = Text.lines . pack 
                
instance Formattable Text where
    format s = s         
instance Formattable Char where
    format = Text.singleton 
instance Formattable Natural where
    format = pack . show        
instance Formattable Int where
    format = pack . show
instance Formattable Word where
    format = pack . show
instance Formattable Integer where
    format = pack . show        
instance Formattable Word8 where
    format = pack . show
instance Formattable Word16 where
    format = pack . show
instance Formattable Word32 where
    format = pack . show
instance Formattable Word64 where
    format = pack . show                
instance Formattable Int8 where
    format = pack . show
instance Formattable Int16 where
    format = pack . show
instance Formattable Int32 where
    format = pack . show
instance Formattable Int64 where
    format = pack . show
instance Formattable Double where
    format = pack . show
instance Formattable Float where
    format = pack . show
instance Formattable CDouble where
    format = pack . show
instance Formattable CFloat where
    format = pack . show
                
instance (Show a) => Formattable (Set a) where
    format x =  braces (pack (show x))
        where braces y = T.append "[" (T.append y "]")

-- Lists of formattable things are formattable        
instance (Formattable a) => Formattable [a] where
    format x = x |> (<$>) format |> Text.concat
            