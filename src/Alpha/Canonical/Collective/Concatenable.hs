-----------------------------------------------------------------------------
-- | Abstractions inspired by list-like structure and operations
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Concatenable
(
    Concatenable(..), 
    Concatenated(..), 

) where
import Alpha.Base
import Alpha.Canonical.Functions

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
class Concatenable a b where
    concat::a -> b -> Concatenated a b

    (++)::a -> b -> Concatenated a b
    (++) x y = concat x y
    infixr 5 ++
    

instance Concatenable [a] [a] where
    concat = (List.++)
        
instance Concatenable (Seq a) (Seq a) where
    concat a b = a <> b
    
instance Concatenable Text Text where
    concat = Text.append    
        
instance Concatenable Text Char where
    concat t c  = Text.pack  [c] |> Text.append t 
    
instance Concatenable Char Text where    
    concat c t  = Text.append (Text.pack [c]) t
    
instance Concatenable Char Char where    
    concat c1 c2  = Text.pack ([c1] List.++ [c2])

    