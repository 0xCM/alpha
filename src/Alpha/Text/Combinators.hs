{-# LANGUAGE FlexibleInstances #-}

module Alpha.Text.Combinators
(    
    dot, dots, space, colon, semi, comma, 
    fslash, bslash, larrow, rarrow,
    enclose, splat, isPrefix, isSuffix,
    leftOfFirst, rightOfLast,ltrim,embrace,
    prefixIfMissing, suffixIfMissing, zpadL, padL, toText,
    hexstring,showBasedInt,bitstring,bitstringN,
    tupelize,
    Text.append, textlen, Text.intersperse
    
)
 where

import qualified Data.Text as Text

import qualified Alpha.Data.List as List
import Numeric(showIntAtBase)
import Alpha.Base
import Alpha.Canonical
import Alpha.Text.Text
import Alpha.Text.Format
import Alpha.Text.Symbols
import Alpha.Data.Maybe
import Alpha.Data.Numbers 

-- | Determines whether text begins with a specified substring
isPrefix::Text -> Text -> Bool
isPrefix = Text.isPrefixOf 

-- | Determines whether text ends with a specified substring
isSuffix::Text -> Text -> Bool
isSuffix = Text.isSuffixOf

-- | Determines whether text is empty
isEmpty::Text -> Bool
isEmpty x = x == Text.empty

-- | Converts an integer to the 'Char' value it reprsents
char::Int -> Char
char = chr

-- | Creates a left-padded string    
padL :: Int -> Text -> Text -> Text
padL n c s
    | textlen s < n  = [padding, s] |> Text.concat
    | otherwise     = s
    where 
        padding = Text.replicate len c    
        len = sub n (textlen s)
        
-- | Creates a left-zero-padded string    
zpadL :: (Integral n) => n -> Text -> Text
zpadL n s = padL (fromIntegral n) "0" s    

toText ::String -> Text
toText s = Text.pack s

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
        [] -> none
        segments -> segments |> List.last |> some

-- | Returns the text that precedes the first occurrence of a specified pattern, if any
leftOfFirst::Text -> Text -> Maybe Text
leftOfFirst match subject 
    = case (Text.splitOn match subject) of
        [] -> none
        segments -> segments |> List.head |> some

-- | Fences content between a left and right bondary
enclose::(Formattable l, Formattable c, Formattable r) => l -> c -> r -> Text
enclose left content right = splat [format left, format content, format right]

-- | Fences content between a left and right brace
embrace::(Formattable a) => a -> Text
embrace content = enclose lbrace content rbrace

instance Container Text Text where
-- | Determines whether text contains a specified substring
    contains match subject = Text.isInfixOf match subject
    singleton x = x

-- | Conditionally prepends the subject with a prefix
prefixIfMissing::Text -> Text -> Text
prefixIfMissing match subject 
    = case isPrefix match subject of
        True -> subject
        False -> concat match subject

-- | Conditionally appends a suffix to the subject
suffixIfMissing::Text -> Text -> Text
suffixIfMissing match subject 
    = case isSuffix match subject of
        True -> subject
        False -> concat match subject

-- | Removes leading occurrence of match and returns the result; otherwise, 
-- returns the input value
ltrim::Text -> Text -> Text
ltrim match subject = 
    case Text.stripPrefix match subject of
        Just x -> x
        _ -> subject

-- | Encodes a finite integral value as a base-16 Text
hexstring :: (Integral n, Show n, FiniteBits n) => n -> Text
hexstring n = showBasedInt 16 n |> zpadL width
    where width = 16 |> div (finiteBitSize n)

-- | Constructs a string representation for an integer 'n' based at 'b' 
showBasedInt:: (Integral n, Show n) => n -> n -> Text
showBasedInt b n = showIntAtBase b intToDigit n "" |> Text.pack

-- | Encodes a finite integral value as a base-2 Text
bitstring :: (Integral n, Show n, FiniteBits n) => n -> Text
bitstring n = showBasedInt 2 n |> zpadL width
    where width = finiteBitSize n

-- | Encodes an integral value as a base-2 Text
bitstringN :: (Integral w, Integral n, Show n) => w -> n -> Text
bitstringN w n = showBasedInt 2 n |> zpadL w

-- Formats a list of formattable items as a tuple
tupelize::(Formattable a) => [a] -> Text
tupelize src =  Text.concat [lparen, content, rparen]
    where content = Text.concat (fmap format (List.intersperse comma flist))
          flist = fmap format src
