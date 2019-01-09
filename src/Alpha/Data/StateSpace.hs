-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Alpha.Data.StateSpace
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
    rule1

) where
import Alpha.Canonical
import Alpha.Data.Graph

-- References: Y2004SAOS, [Y2009MACS,p13]

-- | Represents a potential system *state* or *configuration* 
newtype State l s = State (Vertex l s)
    deriving(Eq, Ord, Generic, Data, Typeable, Functor)
    deriving(Formattable, Show) via (Vertex l s)
instance Newtype(State l s) 

-- | Represents an event classifier for a atomic actions
-- that occur within a context of chronological isolation
newtype Signal l e = Signal (Vertex l e)
    deriving(Eq,Ord,Data,Typeable,Generic,Functor)
    deriving(Formattable,Show) via (Vertex l e)
instance Newtype(Signal l e) 

-- | Prescribes the response of the system upon the observation of 
-- an event classified by 'e' whilst in state s
newtype Rule l s e = Rule (l, State l s, Signal l e, State l s)
    deriving (Eq, Ord, Generic, Data, Typeable)    
instance Newtype(Rule l s e) 

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
newtype TSystem l s e = TSystem ([State l s], [Rule l s e])
    deriving (Eq, Ord,Generic)
instance Newtype(TSystem l s e) 

-- | Represents a pair of actions that may occur simultaneously
newtype MultiAction l a = MultiAction (Signal l a, Signal l a)

-- | Constructs a system state
state::(SemiOrd l, Ord s) => l -> s -> State l s
state label s = State (vertex label s)

-- | Constructs a signal 
signal::(SemiOrd l, Ord e) => l -> e -> Signal l e
signal label a = Signal (vertex label a)

rule::(SemiOrd l, Ord e, Ord s) => l -> State l s -> Signal l e -> State l s -> Rule l s e
rule label s1 e s2 = Rule (label, s1, e, s2)

-- | Constructs an event predicated on the action that occurred, the
-- time it occurred and associated labeled payload
event::(Integral t, SemiOrd l, Ord s, Ord e) => l -> t -> Rule l s e -> Event l s e t
event l t rule = Event (t, rule)

instance Labeled (State l s) l where
    label l s =  wrap $ label l (unwrap s)
    getLabel s = getLabel (unwrap s) 

instance (SemiOrd l, Ord e, Ord s) => Labeled (Rule l s e) l where
    label l (Rule (_,s1,a,s2)) =  rule l s1 a s2
    getLabel  (Rule (l,_,_,_)) = l
    
instance (Formattable3 l s e) => Formattable (Rule l s e) where
    format (Rule (l,s1,e,s2)) 
        = format l <> lspaced Colon <> format (s1,e) <> spaced RMap <> format s2

instance (Formattable3 l s e) => Show (Rule l s e) where
    show = string . format
    
rule1 = rule "r1" (state "state1" 10) (signal "sig1" 20) (state "state2" 30)