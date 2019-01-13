-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Data.Probability
(
    Experiment(..),
    SampleSpace(..),
    SampleEvent(..),
    EventSpace(..),
)
where
import Alpha.Canonical

-- Terminology consistent with Y2008DES

-- | Represents a stochastic configuration
data Experiment a = Experiment
    deriving(Eq, Ord, Generic, Data, Typeable)

-- | Represents the set of all possible outcomes
-- with the context of an 'Experiment'
data SampleSpace a = SampleSpace (Set a)
    deriving(Eq, Ord, Generic, Data, Typeable)

-- | Represents a subset of the 'SampleSpace'. Note that
-- a sample event is defined as a *subset* and not as a point.
data SampleEvent e = SampleEvent (Set e)
    deriving(Eq, Ord, Generic, Data, Typeable)

-- | Represents a collection of 'SampleEvent' that constitute
-- a measurable σ-algebra
data EventSpace e = EventSpace (Set (SampleEvent e))
    deriving(Eq, Ord, Generic, Data, Typeable)

-- | Captures the probability p for an event e as determined 
-- by a 'ProbabilityMeasure'
newtype Probability e p = Probability (SampleEvent e, p)
    deriving(Eq, Ord, Generic, Data, Typeable)

-- | Represents a function that assigns a probability 'p' to 
-- each element of the event space
type ProbabilityMeasure e p = SampleEvent e -> p

-- | Represents the classical probability triple of a nonepty set 
-- (the sample space) together with a sigma algebra (the event space)
-- along with a probability measure (the probability function)
newtype ProbabilitySpace e p = ProbabiltySpace(SampleSpace e, EventSpace e, ProbabilityMeasure e p)

-- | Characterizes probabalistic events
class Stochastic e  where

    -- | The probabilty measure function that assigns the 
    -- probability value p, a real number in the interval [0,1] 
    -- to the event e
    probability::(OrdNum p) => ProbabilityMeasure e p

type instance Individual (SampleEvent e) = Set e
type instance Individual (EventSpace e) = SampleEvent e

instance SigmaAlgebra (EventSpace a)

instance Computable (Probability e p) where
    type Computed (Probability e p) = p

    compute (Probability (_,p)) = p

    