module Alpha.Canonical.Common.Concat
(
    Concatenable(..), 
    Appended(..), 
    Appendable(..),
    
) where
import Alpha.Canonical.Common.Root

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.ByteString as EG
import qualified Data.ByteString.Lazy as LZ




-- | Characterizes a pair whose terms can be related via an append operation
class Concatenable a b where
    type Concatenated a b

    concat::a -> b -> Concatenated a b

    (++)::a -> b -> Concatenated a b
    (++) x y = concat x y
    infixr 5 ++

-- | Defines a family of type-level functions with the intent
-- of projecting nested a sequence of elements to a (relatively)
-- non-nested sequence of elements. An instance need not be
-- Element-invariant
type family Appended a

-- | A list of element lists is appended to produce a list of elements
type instance Appended [[a]] = [a]

-- | A list of elements is appended to produce a single element
type instance Appended [a] = a

-- Classifies a type that can be transformed into an 'Appended' value
class Appendable a where
    append::a -> Appended a

    
instance Concatenable [a] [a] where
    type Concatenated [a] [a] = [a]    
    concat = (List.++)
            
instance Appendable [Text] where    
    append = Text.concat        
    
instance Concatenable Text Text where
    type Concatenated Text Text = Text
    concat = Text.append    
        
instance Concatenable Text Char where
    type Concatenated Text Char = Text
    concat t c  = Text.pack  [c] |> Text.append t 
    
instance Concatenable Char Text where    
    type Concatenated Char Text = Text
    concat c t  = Text.append (Text.pack [c]) t
    
instance Concatenable Char Char where    
    type Concatenated Char Char = Text
    concat c1 c2  = Text.pack ([c1] List.++ [c2])
            