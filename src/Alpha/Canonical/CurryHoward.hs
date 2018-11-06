module Alpha.Canonical.CurryHoward where

import Alpha.Base    
-- Follows the ideas as presented in Wadler's Propositions as Types

--Disjunction A ∨ B corresponds to a disjoint sum A + B, that
--is, a variant with two alternatives. A proof of the proposition
--A ∨ B consists of either a proof of A or a proof of B, including
--an indication of which of the two has been proved. Similarly, a
--value of type A + B consists of either a value of type A or a
--value of type B, including an indication of whether this is a left
--or right summand.
data Disjunct a b = Disjunct (Either a b)

type a :||: b = Disjunct a b


-- Conjunction A & B corresponds to Cartesian product A × B,
-- that is, a record with two fields, also known as a pair. A proof
-- of the proposition A&B consists of a proof of A and a proof of
-- B. Similarly, a value of type A × B consists of a value of type
-- A and a value of type B
data Conjunct a b = Conjunct (a,b)

type a :&: b = Conjunct a b

--Implication A => B corresponds to function space A -> B. A
--proof of the proposition A => B consists of a procedure that
--given a proof of A yields a proof of B. Similarly, a value of
--type A -> B consists of a function that when applied to a value
--of type A returns a value of type B.
data Implies a b = Implies (a->b)

type a :-> b = Implies a b