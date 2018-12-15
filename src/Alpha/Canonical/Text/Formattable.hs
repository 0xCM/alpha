module Alpha.Canonical.Text.Formattable
(
    Formattable(..),
    Packable(..),
    enclose,
    parenthetical, 
    embrace,
    tuplestring,
    spaced,
    suffix,
    prefix,
    text

) where
import Alpha.Base
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations.Tuples
import Alpha.Canonical.Collective.Discrete

import Alpha.Canonical.Text.Asci

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map

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
instance (Show a) => Formattable (Ratio a) where
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

text::(Show s) => s -> Text
text = Text.pack . show
    
enclose::(Formattable l, Formattable c, Formattable r) => l -> r -> c -> Text
enclose left right content = Text.concat [format left, format content, format right]

-- | Fences content between left and right braces
embrace::(Formattable a) => a -> Text
embrace content = enclose LBrace RBrace content 

-- | Fences content between left and right parenthesis
parenthetical::(Formattable a) => a -> Text
parenthetical content = enclose LParen RParen content 
        
-- Formats a list of formattable items as a tuple
tuplestring::(Formattable a) => [a] -> Text
tuplestring src =  Text.concat [LParen, content, RParen] where 
    content = Text.concat (format <$> (weave Comma flist)) where
          flist = format <$> src 

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
        
instance (Formattable a) => Formattable (ItemSet a) where
    format x =  braces (format x)
        where braces y = Text.append "{" (Text.append y "}")

-- Lists of formattable things are formattable        
instance (Formattable a) => Formattable [a] where
    format x = x |> (<$>) format |> Text.concat

instance (Formattable k, Formattable v) => Formattable (Map k v) where
    format m = format $ format  <$> (Map.toList m)
    

type Formattable2 a1 a2 = (Formattable a1, Formattable a2)
type Formattable3 a1 a2 a3 = (Formattable2 a1 a2, Formattable a3)
type Formattable4 a1 a2 a3 a4 = (Formattable3 a1 a2 a3, Formattable a4)
type Formattable5 a1 a2 a3 a4 a5 = (Formattable4 a1 a2 a3 a4, Formattable a5)

instance (Formattable2 a1 a2) => Formattable (Tuple2 a1 a2) where
    format (a1,a2)
        = tuplestring [format a1, format a2]

instance (Formattable3 a1 a2 a3) => Formattable (Tuple3 a1 a2 a3) where
    format (a1,a2,a3)
        = tuplestring [format a1, format a2, format a3]

instance (Formattable4 a1 a2 a3 a4) => Formattable (Tuple4 a1 a2 a3 a4) where
    format (a1,a2,a3,a4) 
        = tuplestring [format a1, format a2, format a3, format a4]

instance (Formattable5 a1 a2 a3 a4 a5) => Formattable (Tuple5 a1 a2 a3 a4 a5) where
    format (a1,a2,a3,a4,a5) 
        = tuplestring [format a1, format a2, format a3, format a4, format a5]
        

        -- separated = elements |> List.intersperse Comma
        -- result = separated |> Text.concat
        
        
        
            