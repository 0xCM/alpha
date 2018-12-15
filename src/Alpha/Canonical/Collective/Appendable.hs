-----------------------------------------------------------------------------
-- | Abstractions inspired by list-like structure and operations
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Appendable
(
    Appendable(..), 
    Prependable(..),
    Concatenated(..), 

) where
import Alpha.Base
import Alpha.Canonical.Operators

import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

type family Concatenated a b

type instance Concatenated [a] [a] = [a]
type instance Concatenated (Seq a) (Seq a) = Seq a   
type instance Concatenated Text Text = Text
type instance Concatenated Text Char = Text
type instance Concatenated Char Text = Text
type instance Concatenated Char Char = Text

-- | Characterizes a pair whose terms can be related via an append operation
class Appendable a b where
    append::a -> b -> Concatenated a b

    (+++) :: a -> b -> Concatenated a b
    (+++) x y = append x y
    infixr 5 +++
    
-- | Characterizes a pair whose terms can be related via an prepend operation    
class Prependable a b where
    prepend::a -> b -> Concatenated a b

instance Appendable [a] [a] where
    append = (List.++)
    
instance Prependable [a] [a] where
    prepend x y = y List.++ x
    
instance Appendable (Seq a) (Seq a) where
    append a b = a <> b

instance Prependable (Seq a) (Seq a) where
    prepend a b = b <> a    
    
instance Appendable Text Text where
    append = Text.append    
        
instance Appendable Text Char where
    append t c  = Text.pack  [c] |> Text.append t 
    
instance Appendable Char Text where    
    append c t  = Text.append (Text.pack [c]) t
    
instance Appendable Char Char where    
    append c1 c2  = Text.pack ([c1] List.++ [c2])

instance Prependable Text Text where
    prepend x y = Text.append y x         
    