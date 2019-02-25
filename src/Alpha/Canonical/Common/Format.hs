-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Format
(
    module X,
    Formattable(..), 
    ToString(..),
    ToLines(..),
    Formattable2, Formattable3, Formattable4, Formattable5,
    trepeat,
    fence,
    parenthetical, 
    embrace,
    dots, 
    dashes, 
    blanks,
    tuplestring,
    setstring,
    pad, 
    lpad, 
    rpad,
    isSuffix, suffix, suffixIfMissing,
    isPrefix, prefix, prefixIfMissing,
    text,
    hexstring,
    showBasedInt,
    bitText,
    bitTextW,    
    isTextEmpty,
    splat, 
    leftOfFirst, 
    rightOfLast,
    ltrim,
    zpadL, 
    padL,
    textlen,     


) where
import Alpha.Canonical.Common.Root as X
import Alpha.Canonical.Common.Asci as X
import Numeric(showIntAtBase)

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.MultiSet as Bag

-- | Characterizes a value that can be rendered in human-readable form
class Formattable a where
    format ::a -> Text            

-- | Characterizes a value that can be converted to a 'String'
class ToString a where
    -- | Convers an 'a' value to a 'String'
    string::a -> String

-- | Characterizes a value that can be converted to a list of 'Text' values
class ToLines a where
    -- | Converts an 'a' value to a list of 'Text' values
    lines::a -> [Text]    

-- | Converts a showable to text    
text::String -> Text
text = Text.pack 

-- | Produces a string by formatting an input value that is enclosed within
-- a formatted boundary
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
          
-- | Formats the input and encloses the result within a space on either side
pad::(Formattable a) => a -> Text
pad t = Blank <> format t <> Blank

-- | Formats the input and encloses left-pads the result with a space
lpad::(Formattable a) => a -> Text
lpad t = Blank <> format t

-- | Formats the input and encloses right-pads the result with a space
rpad::(Formattable a) => a -> Text
rpad t = format t <> Blank

-- | Produces a string formed by concatenating a 
-- specified number of copies of an input string
trepeat::Integral i => i -> Text -> Text
trepeat i t = Text.replicate (fromIntegral i) t

-- | Produces text containing a specified number of blanks
blanks::Integral i => i -> Text
blanks i = trepeat i Blank

suffix::Text -> Text -> Text
suffix = Text.append

prefix::Text -> Text -> Text
prefix a b = Text.append b a

-- | Produces text containing a specified number of "." characters
dots::Integral i => i -> Text
dots i = trepeat i Period

-- | Produces text containing a specified number of "-" characters
dashes::Integral i => i -> Text
dashes i = trepeat i Dash

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

-- | Determines whether text begins with a specified substring
isPrefix::Text -> Text -> Bool
isPrefix = Text.isPrefixOf 

-- | Determines whether text ends with a specified substring
isSuffix::Text -> Text -> Bool
isSuffix = Text.isSuffixOf

-- | Determines whether text is empty
isTextEmpty::Text -> Bool
isTextEmpty x = x == Text.empty

-- | Conditionally prepends the subject with a prefix
prefixIfMissing::Text -> Text -> Text
prefixIfMissing match subject 
    = case isPrefix match subject of
        True -> subject
        False -> match <> subject

-- | Conditionally appends a suffix to the subject
suffixIfMissing::Text -> Text -> Text
suffixIfMissing match subject 
    = case isSuffix match subject of
        True -> subject
        False -> match <> subject

-- | Converts an integer to the 'Char' value it reprsents
char::Int -> Char
char = chr

-- | Creates a left-padded string    
padL :: Int -> Text -> Text -> Text
padL n c s
    | lt' (textlen s) n  = [padding, s] |> Text.concat
    | otherwise     = s
    where 
        padding = Text.replicate len c    
        len = sub' n (textlen s)
        
-- | Creates a left-zero-padded string    
zpadL :: (Integral n) => n -> Text -> Text
zpadL n s = padL (fromIntegral n) "0" s    

replicate::(Integral n) => n -> Text -> Text
replicate n t =  Text.replicate (fromIntegral n) t

-- | Calculates the length of the supplied text
textlen::(Integral n) => Text -> n
textlen t = t |> Text.length |> fromIntegral

-- | Concatenates a list of 'Text' values
splat::[Text] -> Text
splat = Text.concat


-- | Returns the text that follows the last occurrence of a specified pattern, if any
rightOfLast::Text -> Text -> Maybe Text
rightOfLast match subject 
    = case (Text.splitOn match subject) of
        [] -> Nothing
        segments -> segments |> List.last |> Just

-- | Returns the text that precedes the first occurrence of a specified pattern, if any
leftOfFirst::Text -> Text -> Maybe Text
leftOfFirst match subject 
    = case (Text.splitOn match subject) of
        [] -> Nothing
        segments -> segments |> List.head |> Just

-- Determines whether a block of text contains a specified substring
textContains::Text -> Text -> Bool
textContains match subject = Text.isInfixOf match subject    


-- | Removes leading occurrence of match and returns the result; otherwise, 
-- returns the input value
ltrim::Text -> Text -> Text
ltrim match subject = 
    case Text.stripPrefix match subject of
        Just x -> x
        _ -> subject

instance Packable String Text where
    pack = Text.pack
    unpack = Text.unpack
                    
-- *Text formatting
-------------------------------------------------------------------------------    
instance Formattable Text where
    format s = s         

instance Formattable Char where
    format = Text.singleton 

-- *Container formatting
-------------------------------------------------------------------------------    
instance Formattable a => Formattable [a] where
    format x = x |> (<$>) format |> Text.concat
    
instance Formattable a => Formattable (Seq a) where
    format s =  toList s |> (<$>) format |> embrace        

instance (Formattable k, Formattable v) => Formattable (Map k v) where
    format m = format $ format  <$> (Map.toList m)    
    
-- *Numeric formatting
-------------------------------------------------------------------------------    
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

-- *Tuple formatting
-------------------------------------------------------------------------------    
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
    
-- *Other formatting
-------------------------------------------------------------------------------            
instance Formattable TyConInfo where
    format (TyConInfo (_,mod,ctor)) = mod <> fence LParen RParen ctor 

instance Show TyConInfo where
    show = Text.unpack . format    

instance  (Formattable v, Faceted f v) => Formattable (FacetValue f  v) where
    format (FacetValue v) =  format v
        
instance Formattable Variance where
    format (Covariant) = "*"
    format (Contravariant) = "^"    

-------------------------------------------------------------------------------
-- * ToString instances
-------------------------------------------------------------------------------    
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
        