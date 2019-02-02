-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Linear.Eigen
(
    module X,
    Eigenvector(..),
    Eigenvalue(..),
    Eigenpair(..),
    Spectrum(..)
)
where
import Alpha.Canonical
import Alpha.Linear.Operators as X


-- | Represents an eigenvector of a specified linear operator
-- See https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors
newtype Eigenvector n k = Eigenvector (k, (LinearOperator n k))
    deriving (Eq,Generic,Data,Typeable)

-- | Represents an eigenvalue of a specified linear operator
-- See https://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors
newtype Eigenvalue n k = Eigenvalue (k, (LinearOperator n k))
    deriving (Eq,Generic,Data,Typeable)

-- | Represents an eigenvalue k together with an eigenvector v
-- of a specified linear operator A such that Av = kv
newtype Eigenpair n k  = Eigenpair (Eigenvector n k, Eigenvalue n k)
    deriving (Eq,Generic,Data,Typeable)

-- | Represents the spectrum of a linear operator, i. e. the set of
-- operator eigenvalues
newtype Spectrum n k = Spectrum [Eigenvalue n k]
    deriving (Eq,Generic,Data,Typeable)