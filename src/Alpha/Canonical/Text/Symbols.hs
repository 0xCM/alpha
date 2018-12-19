-----------------------------------------------------------------------------
-- | Common symbols
-- Copyright   :  (c) Chris Moore, 2018 + Contributors per license specification
-- License     :  MIT/BSD
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------

module Alpha.Canonical.Text.Symbols where

import Data.Text
import Alpha.Base


-- | Produces "."
dot::Text
dot = "."


-- | Produces "-"
dash::Text
dash = "-"


-- | Produces " "
space::Text
space = " "


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

-- | Produces "⫴"
pipe3::Text
pipe3 = "⫴"

-- | Produces "¬" (logical not)
lnot::Text
lnot = "¬"

-- | Produces "∧" (logical and)
pattern And = "∧"::Text
-- | Produces "⊼" (logical not(a && b))
pattern NAnd = "⊼"::Text
pattern Or = "∨"::Text
pattern XOr = "⊻"::Text
pattern Neq = "≠"::Text
pattern NOr = "⊽"::Text