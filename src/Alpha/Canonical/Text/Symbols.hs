-----------------------------------------------------------------------------
-- | Common symbols
-- Copyright   :  (c) Chris Moore, 2018 + Contributors per license specification
-- License     :  MIT/BSD
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------

module Alpha.Canonical.Text.Symbols where
import Alpha.Canonical.Common
import Data.Text

-- | Produces "⊛"
pattern CircStar = "⊛"::Text

-- | Produces the empty set symbol ∅
pattern EmptySet = "∅"::Text

-- | Produces the logical and symbol ∧
pattern And = "∧"::Text

-- | Produces the logical or symbol "∨" 
pattern Or = "∨"::Text

pattern XOr = "⊻"::Text

pattern NOr = "⊽"::Text

pattern Neq = "≠"::Text

-- | Produces an upward arrow symbol ↑
pattern UArrow = "↑"::Text

-- | Produces a downward arrow symbol ↓
pattern DArrow = "↓"::Text

