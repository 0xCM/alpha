-----------------------------------------------------------------------------
-- | Common symbols
-- Copyright   :  (c) 0xCM, 2018 + Contributors per license specification
-- License     :  MIT/BSD
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------

module Alpha.Text.Symbols where

import Data.Text
import Alpha.Base
import Alpha.Canonical
import Alpha.Text.Asci

-- | Produces "."
dot::Text
dot = "."

-- Produces "..."
dots::Text
dots = "..."

-- | Produces " "
space::Text
space = " "

-- | Produces ":"
colon::Text
colon = ":"

-- | Produces ";"
semi::Text
semi = ";"

-- | Produces ","
comma::Text
comma = ","

-- | Produces "+"
plus::Text
plus = "+"

-- | Produces "->"
rarrow::Text
rarrow = "->"

-- | Produces "<-"
larrow::Text
larrow = "<-"

-- | Produces "["
lbracket::Text
lbracket = "["

-- | Produces "]"
rbracket::Text
rbracket = "]"

-- | Produces "{"
lbrace::Text
lbrace = "{"

-- | Produces "}"
rbrace::Text
rbrace = "}"

-- | Produces "{"
lparen::Text
lparen = LParen

-- | Produces "}"
rparen::Text
rparen = RParen

-- | Produces "/"
fslash::Text
fslash = "/"

-- | Produces "\"
bslash::Text
bslash = "\\"

-- | Produces "⊛"
circstar::Text
circstar = "⊛"

-- | Produces "∅"
emptyset::Text
emptyset = "∅"

-- | Produces "⁂
asterism::Text
asterism = "⁂"

-- | Produces "⧻
plus3::Text
plus3 = "⧻"

-- | Produces "|"
pipe::Text
pipe = "|"

-- | Produces "⫴"
pipe3::Text
pipe3 = "⫴"

-- | Produces "¬" (logical not)
lnot::Text
lnot = "¬"

-- | Produces "∧" (logical and)
land::Text
land = "∧"

-- | Produces "⊼" (logical not(a && b))
nand::Text
nand = "⊼"

or::Text
or = "∨"

xor::Text
xor = "⊻"

nor::Text
nor = "⊽"

neq::Text
neq = "≠"

