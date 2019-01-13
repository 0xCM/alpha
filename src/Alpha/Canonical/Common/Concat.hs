-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
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
import qualified Data.Tree as Tree



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
type instance Appended [[a]] = [a]
type instance Appended [a] = a
type instance Appended (Tree a) = [a]

-- Classifies a type that can be transformed into an 'Appended' value
class Appendable a where
    append::a -> Appended a



instance Appendable (Tree a) where
    append = Tree.flatten

    
instance Concatenable [a] [a] where
    type Concatenated [a] [a] = [a]    
    concat = (List.++)
            
instance Appendable [[a]] where
    append = List.concat    
    
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
            
instance Concatenable (Seq a) (Seq a) where
    type Concatenated (Seq a) (Seq a) = Seq a   
    concat a b = a <> b
