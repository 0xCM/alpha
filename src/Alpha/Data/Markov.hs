module Alpha.Data.Markov
(
    DecisionProcess(..),
    StateAction(..),
    SystemState(..),
    Occurrence(..),

    action, occurrence,
)
where
import Alpha.Canonical

-- | Represents a potential state in the system
newtype SystemState s = SystemState s
    deriving (Eq, Ord)

-- | Represents all potential states in the system
newtype StateSpace a = StateSpace (FneSet (SystemState a))
    deriving (Eq, Ord, Associated, Unionizable, Differential, Intersectable, Containment, Existential, Universal, Queryable)

type instance Individual (StateSpace a) = SystemState a    

-- | Represents a potential action  in the system
newtype SystemAction a = SystemAction a

-- | Represents a system action 'a' that is acheivable from a state 's'
newtype StateAction s a = StateAction (SystemState s, SystemAction a)

-- | Represents the occurrence of a system action a in state s at time t
newtype Occurrence t s a = Occurrence (t, StateAction s a)

-- | Represents a Markov decision process
newtype DecisionProcess s a = DecisionProcess ([SystemState s], [StateAction s a])

-- | Represents the liklihood of the occurrence of some event
newtype Probability e p = Probability (e, p)
    deriving(Eq, Ord)

-- | Represents a transition from one state to another    
newtype Transition t s a = Transition (Occurrence t s a, SystemState s)

-- | Characterizes probabalistic events
class Stochastic e  where
    -- | Assigns the probability of the event e to the value p
    probability::(OrdNum p) => e -> p -> Probability e p
            
instance Stochastic (Transition t s a) where
    probability t p = Probability (t, check p) where
        
        check::(OrdNum p) => p -> p
        check p = case (p >= 0 && p <= 1) of
                         True -> p
                         _ -> error "Out of bounds" 

-- | Defines an action that is associated with a specified state
action::s -> a -> StateAction s a
action s a = StateAction (SystemState s, SystemAction a)

occurrence::(Integral t) => t -> s -> a -> Occurrence t s a
occurrence t s a = Occurrence (t, (action s a))

                