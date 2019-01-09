-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Format
(
    Formattable2, Formattable3, Formattable4, Formattable5,
    fence,
    parenthetical, 
    embrace,
    dots, 
    dashes, 
    arrow,
    spaces,
    tuplestring,
    setstring,
    spaced, 
    lspaced, 
    rspaced,
    suffix,
    prefix,
    text,
    hexstring,
    showBasedInt,
    bitText,
    bitTextW

) where
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Asci
import Alpha.Canonical.Common.TextUtil(zpadL)
import Alpha.Canonical.Common.Synonyms
import Numeric(showIntAtBase)

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.MultiSet as Bag


                
-- | Converts a showable to text    
text::String -> Text
text = Text.pack 
    
fence::(Formattable l, Formattable c, Formattable r) => l -> r -> c -> Text
fence left right content = Text.concat [format left, format content, format right]

-- | Formats a list of values and ensconces the result between left and right braces
embrace::(Formattable a) => [a] -> Text
embrace items = format <$> items |> List.intersperse Comma |> Text.concat |> fence LBrace RBrace

-- | Fences content between left and right parenthesis
parenthetical::(Formattable a) => a -> Text
parenthetical content = fence LParen RParen content 
        
-- Formats a list of formattable items as a tuple
tuplestring::(Formattable a) => [a] -> Text
tuplestring src =  Text.concat [LParen, content, RParen] where 
    content = Text.concat (format <$> (List.intersperse Comma flist)) where
          flist = format <$> src 

-- Formats a list of formattable items as a set
setstring::(Formattable a) => [a] -> Text  
setstring s = fence LBrace RBrace (format elements) where
    elements =  weave Comma (format <$> s)
          
-- | Surrounds the input text within a space on each side
spaced::Text -> Text
spaced t = Space <> t <> Space

lspaced::Text -> Text
lspaced t = Space <> t

rspaced::Text -> Text
rspaced t = t <> Space

suffix::Text -> Text -> Text
suffix = Text.append

prefix::Text -> Text -> Text
prefix a b = Text.append b a

repeat::Integral i => i -> Text -> Text
repeat i t = Text.replicate (fromIntegral i) t

-- | Produces text containing a specified number of "." characters
dots::Integral i => i -> Text
dots i = repeat i Period

-- | Produces text containing a specified number of "-" characters
dashes::Integral i => i -> Text
dashes i = repeat i Dash

arrow::Integral i => i -> Text
arrow i = dashes i <> Greater

-- | Produces text containing a specified number of spaces
spaces::Integral i => i -> Text
spaces i = repeat i Space

-- | Encodes a finite integral value as a base-16 Text
hexstring :: (Show n, Integral n, FiniteBits n) => n -> Text
hexstring n = showBasedInt 16 n |> zpadL width
    where width = 16 |> div' (finiteBitSize n)

-- | Constructs a string representation for an integer 'n' based at 'b' 
showBasedInt::(Integral n, Show n) => n -> n -> Text
showBasedInt b n = showIntAtBase b intToDigit n "" |> Text.pack

-- | Encodes a finite integral value as a base-2 Text
bitText ::(Show n, Integral n, FiniteBits n) => n -> Text
bitText n = showBasedInt 2 n |> zpadL width
    where width = finiteBitSize n

-- | Encodes an integral value as a base-2 Text
bitTextW :: (Integral w, Integral n, Show n) => w -> n -> Text
bitTextW w n = showBasedInt 2 n |> zpadL w

instance Packable String Text where
    pack = Text.pack
    unpack = Text.unpack
                    
instance  (Formattable v, Faceted f v) => Formattable (FacetValue f  v) where
    format (FacetValue v) =  format v
    

instance Formattable Text where
    format s = s         
instance Formattable Char where
    format = Text.singleton 
instance Formattable Natural where
    format = Text.pack . show
instance Formattable Int where
    format = Text.pack . show
instance Formattable Word where
    format = Text.pack . show
instance Formattable Integer where
    format = Text.pack . show
instance (Show a) => Formattable (Ratio a) where
    format = Text.pack . show
instance Formattable Word8 where
    format = Text.pack . show
instance Formattable Word16 where
    format = Text.pack . show
instance Formattable Word32 where
    format = Text.pack . show
instance Formattable Word64 where
    format = Text.pack . show
instance Formattable Int8 where
    format = Text.pack . show
instance Formattable Int16 where
    format = Text.pack . show
instance Formattable Int32 where
    format = Text.pack . show
instance Formattable Int64 where
    format = Text.pack . show
instance Formattable Double where
    format = Text.pack . show
instance Formattable Float where
    format = Text.pack . show
instance Formattable CDouble where
    format = Text.pack . show
instance Formattable CFloat where
    format = Text.pack . show

instance (Formattable a) => Formattable [a] where
    format x = x |> (<$>) format |> Text.concat
    
instance Formattable Variance where
    format (Covariant) = "*"
    format (Contravariant) = "^"    

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
    
instance (Formattable k, Formattable v) => Formattable (Map k v) where
    format m = format $ format  <$> (Map.toList m)    
        
instance Formattable TyConInfo where
    format (TyConInfo (_,mod,ctor)) = mod <> fence LParen RParen ctor 

instance Show TyConInfo where
    show = Text.unpack . format
    