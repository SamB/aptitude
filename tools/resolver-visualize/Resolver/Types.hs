-- | The core data types used to represent packages, versions, and
-- other problem resolver structures.

module Resolver.Types where

import Data.ByteString.Char8(ByteString)
import Data.Set
import Data.Map

-- | Represents a package.
--
-- The whole resolver database is not available when parsing the log,
-- so packages can't contain the list of their versions.
data Package = Package { pkgName :: ByteString }
               deriving(Ord, Eq, Show)

-- | Represents a single version of a package.
data Version = Version { -- | The package that this version is
                         -- associated with.
                         verPkg :: Package,
                         -- | A unique string that identifies this
                         -- version among the versions of the package.
                         verName :: ByteString }
               deriving(Ord, Eq, Show)

-- | Represents a single dependency.
--
-- TODO: the resolver should state whether a dependency is "soft" when
-- logging it.  (e.g.: pkg S-> { v1 v2 })
data Dep = Dep { -- | The version that declares this dependency.
                 depSource :: Version,
                 -- | A list of versions that solve this dependency.
                 depSolvers :: [Version] }
         deriving(Ord, Eq, Show)


-- | Represents a choice made by the dependency resolver.
data Choice = InstallVersion { -- | The version to install.
                               choiceVer :: Version,
                               -- | The dependency that triggered this
                               -- installation.
                               --
                               -- Will be Nothing if it is not known.
                               choiceVerReason :: Maybe Dep,
                               -- | True if this choice was made to
                               -- alter the source of the dependency.
                               -- Nothing if it is not known.
                               choiceFromDepSource :: Maybe Bool }
            -- ^ Install a single version.
            -- | Leave a soft dependency unresolved.
            | BreakSoftDep { -- | The dependency that was not
                             -- resolved.
                             choiceDep :: Dep }
              deriving(Ord, Eq, Show)


-- | Represents a single solution created by the dependency resolver.
data Solution = Solution { -- | The choices made in this solution.
                           -- Each choice is mapped to an integer
                           -- representing its order in the
                           -- solution, or to Nothing if the order
                           -- is unknown.
                           solChoices :: Map Choice (Maybe Int),
                           -- | The versions that may not be
                           -- installed in this solution.
                           solForbiddenVersions :: Set Version,
                           -- | Dependencies that are broken under
                           -- this solution.
                           solBrokenDeps :: Set Dep,
                           -- | The score of this solution.
                           solScore :: Integer,
                           -- | The tier of this solution.
                           solTier  :: Tier }
              deriving(Ord, Eq, Show)

newtype Tier = Tier { tierLevel :: Integer } deriving(Ord, Eq)
maximumTier = Tier 2147483647
conflictTier = maximumTier
deferTier = Tier (tierLevel conflictTier - 1)
alreadyGeneratedTier = Tier (tierLevel deferTier - 2)
minimumTier = Tier (-2147483648)

-- The Show instance mainly special-cases the special tiers so they
-- get pretty-printed.
instance Show Tier where
    showsPrec _ tier
        | tier == conflictTier         = ("T:conflict"++)
        | tier == deferTier            = ("T:defer"++)
        | tier == alreadyGeneratedTier = ("T:redundant"++)
        | otherwise =  ('T':) . shows (tierLevel tier)

-- | Represents a promotion to a tier.
data Promotion = Promotion { -- | The choices that produced this promotion.
                             promotionChoices :: Set Choice,
                             -- | The tier of this promotion.
                             promotionTier :: Tier }
               deriving(Ord, Eq, Show)

