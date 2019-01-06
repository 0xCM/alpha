{-# LANGUAGE OverloadedLists #-}
module Alpha.Data.PetriNet
(
    -- Vocabulary
    PetriNet, 
    PetriState, 
    PetriInput,

    -- Construction
    petriNet,
    
    -- Analysis
    
    
)
where

import Alpha.Canonical
import qualified Data.Set as Set
import qualified Data.List as List

-- From: The Application of Petri Nets to Workflow Management

-- A petri net is a directed bipartite graph where node types are partitioned 
-- between *places*, typically depicted as circles, and *transitions*, typically
-- depicted by rectangles
-- A place p is called an *input place* of a *transition* t if there exists
-- an edge from p to t
-- A place p is called an *output place* of a *transition* t if there exists
-- and edge from t to p

type Place p = (Word, p)

-- | Represents a node in a petri net
data PetriNode p t 
    = Place Word p 
    | Transition t
    deriving (Show, Eq, Ord)

-- | The input for a given transition (determined by the list
-- of non-vanishing placements)
data PetriInput p t = PetriInput t [Place p]

-- The state of a petri net is defined to be the distribution of tokens over places    
-- wich we intepret as a linear combination of places...or as a list of pairs
data PetriState p = PetriState [Place p]    

-- | Represents an edge in a petri net    
data Edge p t = Edge (PetriNode p t) (PetriNode p t)
    deriving (Show, Eq, Ord)

-- | Represents a petri net
data PetriNet p t = PetriNet (Set (Edge p t))
    deriving (Eq, Ord)

type instance Individual (PetriNet p t) = Edge p t

-- | Determines whether a vertex represents a place    
isPlace::PetriNode p t -> Bool
isPlace(Place _ _) = True
isPlace(_) = False

-- | Determines whether a vertex represents a transition
isTransition::PetriNode p t -> Bool
isTransition = not . isPlace

-- | Defines a place vertex in a petri net
place::Word -> p -> PetriNode p t
place tokens p = Place tokens p

-- | Defines a transition in a petri net
transition::t -> PetriNode p t
transition = Transition

-- | Defines an edge in a petri net
edge::PetriNode p t -> PetriNode p t -> Edge p t
edge src dst = Edge src dst

-- | Constructs a petri net from a list of edges
petriNet::(Ord p, Ord t) => [Edge p t] -> PetriNet p t
petriNet = PetriNet . set

-- | Extracts the vertrices from a petri net
vertices::(Ord p, Ord t) => PetriNet p t -> [PetriNode p t]
vertices (PetriNet edges) = result where
    result = edges |> toList |> fmap (\(Edge v1 v2) -> [v1,v2] ) |> unions
                         

-- | Calculates the state of a petri net
state::(Ord p, Ord t) => PetriNet p t -> PetriState p
state pn = vertices pn  |> filter isPlace 
                        |> toList
                        |> fmap (\(Place tokens p) -> (tokens,p)) 
                        |> filter (\(t,p) -> t /= 0)
                        |> PetriState

-- | Calculates the input places for a given transition
inputs::(Ord p, Ord t) => t -> PetriNet p t -> PetriInput p t
inputs t (PetriNet edges) = edges 
                         |> toList
                         |> filter (\(Edge (Place tokens p ) (Transition t')) -> t == t')
                         |> fmap (\(Edge (Place tokens p ) _) ->  (tokens,p) )                         
                         |> PetriInput t

-- | Determines whether a transition is enabled which by definition is True
-- if each input place contains one or more tokens
enabled::(Ord t, Ord p) => t -> PetriNet p t -> Bool                  
enabled t pn = length all == length nonzero  where 
    (PetriInput _ all) = inputs t pn
    nonzero = all |> filter (\(tokens,p) -> tokens /= 0)