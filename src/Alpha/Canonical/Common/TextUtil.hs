-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module Alpha.Canonical.Common.TextUtil
(    
    splat, isPrefix, isSuffix,
    leftOfFirst, rightOfLast,ltrim,
    zpadL, padL,
    textlen, 
    suffixIfMissing, 
    prefixIfMissing, 
    
)
 where
import Alpha.Base
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Asci
import Alpha.Canonical.Common.Symbols
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Aeson.Encode.Pretty

import qualified Data.Text as Text
import qualified Data.List as List
import Numeric(showIntAtBase)


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
    | lt' (textlen s) n  = [padding, s] |> Text.concat
    | otherwise     = s
    where 
        padding = Text.replicate len c    
        len = sub' n (textlen s)
        
-- | Creates a left-zero-padded string    
zpadL :: (Integral n) => n -> Text -> Text
zpadL n s = padL (fromIntegral n) "0" s    

-- toText ::String -> Text
-- toText s = Text.pack s

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

-- | Removes leading occurrence of match and returns the result; otherwise, 
-- returns the input value
ltrim::Text -> Text -> Text
ltrim match subject = 
    case Text.stripPrefix match subject of
        Just x -> x
        _ -> subject


