-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Alpha.Data.Machinery
(
    State(..),
    TSystem(..),
    Signal(..),
    MultiAction(..),
    Rule(..),
    Event(..),    
    Reaction(..),
    state,
    signal,
    event,

) where
import Alpha.Canonical
import Alpha.Data.Graph
import Alpha.Data.Grammar

-- References: Y2004SAOS, [Y2009MACS,p13]

-- | Represents a potential system *state* or *configuration* 
newtype State l s = State (l,s)
    deriving(Eq, Ord, Generic, Data, Typeable, Functor)
    deriving(Formattable, Show)
instance Newtype(State l s) 

-- | Represents the initial state for a given machine
newtype StartState l s = StartState (State l s)
    deriving(Eq, Ord, Generic, Data, Typeable)
    deriving (Formattable, Show) via (State l s)

-- | Represents the operational states a given machine,
-- i. e. those other than the initial and final states    
newtype OpStates l s = OpStates (Map l (State l s))    
    deriving(Eq, Ord, Generic, Data, Typeable,Functor)
    deriving (Formattable, Show)

-- | Represents collection of states in which a machine may terminate
newtype EndStates l s = EndStates (Map l (State l s))
    deriving(Eq, Ord, Generic, Data, Typeable,Functor)
    deriving (Formattable, Show)

-- | Represents the collection of states within the purview of a given machine
newtype StateSets l s = StateSets (StartState l s, OpStates l s,  EndStates l s)
    deriving(Eq, Ord, Generic, Data, Typeable)

-- | Represents an event classifier for a atomic actions
-- that occur within a context of chronological isolation
newtype Signal l e = Signal (l,e)
    deriving(Eq,Ord,Data,Typeable,Generic,Functor)
    deriving(Formattable,Show)
instance Newtype(Signal l e) 

-- | Represents the collection of signals in the vocabulary of a given machine
newtype Signals l e = Signals (Map l (Signal l e))
    deriving(Eq, Ord, Generic, Data, Typeable, Functor)
    
-- | Prescribes the response of the system upon the observation of 
-- an event classified by 'e' whilst in state s
newtype Rule l s e = Rule (l, State l s, Signal l e, State l s)
    deriving (Eq, Ord, Generic, Data, Typeable)    
instance Newtype(Rule l s e) 

-- | Prescribes a set of coherent rules to govern a given machine
newtype Ruleset l s e = Ruleset (Map l (Rule l s e))
    deriving(Eq, Ord, Generic, Data, Typeable)

-- | Represents the occurrence of  a signal at a specific
-- time and associated with the 'Rule' for which it is applicable
newtype Event l s e t = Event (t, Rule l s e)
    deriving (Eq, Ord, Generic, Data, Typeable)
    deriving(Formattable,Show)
instance Newtype(Event l t s e) 

-- | Associates the occurrence of a 'Event' with the target state
-- If the event induced no change in state, then the source and target states
-- will be identical
newtype Reaction l s e t = Reaction (Event l t s e, State l s)
    deriving (Eq, Ord, Generic, Data, Typeable)
instance Newtype(Reaction l s e t) 

-- | Represents a a t-ordered sequence of reactions that describe
-- the behavior of a systm over time
newtype ReactionChain l s e t = ReactionChain [Reaction l s e t]
    deriving (Eq, Ord, Generic, Data, Typeable)

-- | Represents a transition system that:
-- 1. Specifies all potential system states
-- 2. Specifies the rules that govern reactions/transitions
newtype TSystem l s e = TSystem [Rule l s e]
    deriving (Eq, Ord,Generic)
instance Newtype(TSystem l s e) 

-- | Represents a pair of actions that may occur simultaneously
newtype MultiAction l a = MultiAction (Signal l a, Signal l a)

type instance Individual (OpStates l s) = State l s
type instance Individual (EndStates l s) = State l s        
type instance Individual (StateSets l s) = State l s        
type instance Individual (Signals l e) = Signal l e    
type instance Individual (Ruleset l s e) = Rule l s e    

-- | Constructs a system state
state::(SemiOrd l, Ord e) => l -> s -> State l s
state label s = State $ (label, s)

startState::(SemiOrd l, Ord e) => State l s -> StartState l s
startState = StartState

-- | Constructs a signal 
signal::(SemiOrd l, Ord e) => l -> e -> Signal l e
signal label e = Signal $ (label, e)

rule::(SemiOrd l, Ord e, Ord s) => l -> State l s -> Signal l e -> State l s -> Rule l s e
rule label s1 e s2 = Rule (label, s1, e, s2)

-- | Constructs an event predicated on the action that occurred, the
-- time it occurred and associated labeled payload
event::(Integral t, SemiOrd l, Ord s, Ord e) => l -> t -> Rule l s e -> Event l s e t
event l t rule = Event (t, rule)

newtype TuringRule l s e = TuringRule (Rule l s e, Sign)    

newtype TuringMachine l s e = TuringMachine (StateSets l s, Signals l e) 

instance (SemiOrd l, Ord s) => Vertexed l s (State l s) where
    vertex l s = State (l,s)

instance Labeled (State l s) l where
    label l (State (_,s)) =  State (l,s)
    getLabel (State (l,_)) = l
    
instance (SemiOrd l, Ord e, Ord s) => Labeled (Rule l s e) l where
    label l (Rule (_,s1,a,s2)) =  rule l s1 a s2
    getLabel  (Rule (l,_,_,_)) = l
    
instance (Formattable3 l s e) => Formattable (Rule l s e) where
    format (Rule (l,s1,e,s2)) 
        = format l <> lpad Colon <> format (s1,e) <> pad RMap <> format s2

instance (Formattable3 l s e) => Show (Rule l s e) where
    show = string . format
    
instance (SemiOrd l, Ord e, Nullary l, Nullary e) => Nullary (Signal l e) where
    zero = signal zero zero

instance Labeled (StartState l s) l where
    label l (StartState (State(_,s))) =  StartState (State (l,s))
    getLabel (StartState (State(l,_))) = l
    
instance (SemiOrd l, Ord s) => Discrete (StateSets l s) where
    individuals (StateSets ((StartState s), (OpStates op), EndStates end)) 
        = [s] <> (associated op) <> (associated end)

instance (SemiOrd l, Ord s) => Indexable (OpStates l s) where
    type Indexer (OpStates l s) = l
    idx (OpStates states) ix = (states !! ix) |> snd

instance (SemiOrd l, Ord s) => Indexable (EndStates l s) where
    type Indexer (EndStates l s) = l
    idx (EndStates states) ix = (states !! ix) |> snd

instance (SemiOrd l, Ord e) => Indexable (Signals l e) where
    type Indexer (Signals l e) = l
    idx (Signals signals) ix = (signals !! ix) |> snd
        
            