-----------------------------------------------------------------------------
-- | Defines predicate operators and types
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Relations.Functions
(
    Dom(..),
    Cod(..),
    Composition(..), 
    Compositional(..), 
    Function(..),
    Functional(..)
    
) where
import Alpha.Base
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations.Related
import Control.Category(Category(..))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List


-- | Defines a collection of ordered pairs, no two of which have the
-- same first term and is thus, by definition, a function    
newtype PairFunc a b = PairFunc (Map a b)
    deriving (Generic,Eq,Ord,Show)
instance Newtype (PairFunc a b)

newtype WrapFunc a b = WrapFunc (Func a b)
    deriving (Generic)
instance Newtype (WrapFunc a b)

-- | Defines a family of types that specify function composition
-- Mor precisely, given a function g whose domain coincides with
-- the codomain of a function f the 'Composition' type of g and f
-- is the type of the function h:Dom f -> Cod g where h = g . f
type family Composition g f
type instance Composition (Func b c) (Func a b) = Func a c
type instance Composition (Map b c) (Map a b) = Map a c
type instance Composition (WrapFunc b c) (WrapFunc a b) = WrapFunc a c

-- | Characterizes function composition    
class Compositional g f where
    compose::g -> f -> Composition g f
    
-- Defines a family of types that specify function domains
type family Dom f 
type instance Dom (Func a b) = a
type instance Dom (Map a b) = a
type instance Dom (WrapFunc a b) = a

-- Defines a family of types that specify function codomains
type family Cod f     
type instance Cod (Func a b) = b
type instance Cod (Map a b) = b
type instance Cod (WrapFunc a b) = b

-- | Defines a family of types that specify function definitions
data family Function f      
data instance Function (Func a b) = FreeFunction (a -> b)
data instance Function (Map a b)  = PairFunction (Map a b)
data instance Function (WrapFunc a b)  = WrapFunction (WrapFunc a b)

class Functional f where    
    func::Function f -> Func (Dom f) (Cod f)

instance forall f a b. (Ord a, a ~ Dom f, b ~ Cod f, f ~ Map a b) => Functional (Map a b) where    
    func (PairFunction m) = g
        where
            g::Dom f -> Cod f
            g x = m Map.! x

instance forall f a b. (a ~ Dom f, b ~ Cod f, f ~ Map a b) => Functional (Func a b) where    
    func (FreeFunction f) = f

instance forall f a b. (a ~ Dom f, b ~ Cod f, f ~ WrapFunc a b) => Functional (WrapFunc a b) where    
    func (WrapFunction f) = unwrap f
                        
instance (Ord a, Ord b) => Compositional (Map b c) (Map a b) where
    compose mg mf= mh where
        f = func' mf
        g = func' mg
        h = g . f
        z = (\a -> (a, h a)) <$> Map.keys mf
        mh = Map.fromList z
        func' m =  (Map.!) m

instance Compositional (WrapFunc b c) (WrapFunc a b) where
    compose g f = wrap $ (unwrap g) . (unwrap f)
        
instance Category WrapFunc where
    id = wrap (\x -> x)
    (.) = compose        