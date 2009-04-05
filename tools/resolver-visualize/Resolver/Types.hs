-- | The core data types used to represent packages, versions, and
-- other problem resolver structures.

module Resolver.Types where

import Data.ByteString.Char8(ByteString)
import Data.Set(Set)
import Data.List
import Data.Map(Map)

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
                 depSolvers :: [Version],
                 depIsSoft :: Bool}
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
              deriving(Show)

-- Custom Eq and Ord instances here to adjust the order in which the
-- parts of the Solution structure are compared.  As in the C++ code,
-- this exploits the fact that scores are very nearly unique among the
-- solutions in a particular run to avoid comparing sets most of the
-- time.
instance Eq Solution where
    sol1 == sol2 =
        solScore sol1 == solScore sol2 &&
        solTier sol1 == solTier sol2 &&
        solBrokenDeps sol1 == solBrokenDeps sol2 &&
        solForbiddenVersions sol1 == solForbiddenVersions sol2 &&
        solChoices sol1 == solChoices sol2

instance Ord Solution where
    sol1 `compare` sol2 =
        foldr combine EQ [solScore sol1 `compare` solScore sol2,
                          solTier sol1 `compare` solTier sol2,
                          solBrokenDeps sol1 `compare` solBrokenDeps sol2,
                          solForbiddenVersions sol1 `compare` solForbiddenVersions sol2,
                          solChoices sol1 `compare` solChoices sol2]
            where combine EQ o2 = o2
                  combine o1 _  = o1

maximumTierNum = 2147483647
minimumTierNum = -2147483648
newtype Tier = Tier { tierLevels :: [Integer] } deriving(Ord, Eq)
maximumTier = Tier [maximumTierNum]
conflictTier = maximumTier
alreadyGeneratedTier = Tier [maximumTierNum - 1]
deferTier = Tier [maximumTierNum - 2]
minimumTier = Tier [minimumTierNum]

-- The Show instance mainly special-cases the special tiers so they
-- get pretty-printed.
instance Show Tier where
    showsPrec _ tier
        | tier == conflictTier         = ("T:conflict"++)
        | tier == deferTier            = ("T:defer"++)
        | tier == alreadyGeneratedTier = ("T:redundant"++)
        | tier == minimumTier          = ("T:minimum"++)
        | otherwise =  case tier of
                         Tier [num] -> ('T':) . shows (num)
                         Tier nums  -> ("T("++) . foldr (.) id (intersperse (", "++) (map shows nums)) . (')':)

-- | Represents a promotion to a tier.
data Promotion = Promotion { -- | The choices that produced this promotion.
                             promotionChoices :: Set Choice,
                             -- | The tier of this promotion.
                             promotionTier :: Tier }
               deriving(Ord, Eq, Show)

