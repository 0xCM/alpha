-----------------------------------------------------------------------------
-- | Provides access to GHC directory paths
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-----------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}

module Alpha.GHC.Paths where
import Data.Eq
import Alpha.Text
import Alpha.System.Files
import Alpha.Canonical
import qualified Data.Text as T
import GHC.Show(Show)

import qualified GHC.Paths as GP

-- | Specifies noteworthy GHC file system locations
data GhcPaths = GhcPaths {
    -- | The path to the compiler executeable, ghc.exe
    compiler :: FilePath,
    -- | The path to the package tool, ghc-pkg.exe
    packtool :: FilePath,
    -- | The path to the root GHC lib directory
    libs :: FilePath,
    -- | The path to the base documentation directory, e.g., ../doc/html/libraries/base-4.11.1.0
    basedocs :: FilePath
} deriving(Eq,Show)

-- | Returns GHC path information for the compiler currently in-use
ghcpaths :: GhcPaths
ghcpaths = GhcPaths {
    compiler =  file <| T.pack GP.ghc,
    packtool =  file <| T.pack GP.ghc_pkg,
    libs =      file <| T.pack GP.libdir,
    basedocs =  file <| T.pack GP.docdir
}