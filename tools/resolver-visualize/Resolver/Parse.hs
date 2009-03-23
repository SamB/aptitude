-- | Core parsing routines to parse various structures.
--
-- Note: various assumptions are made in this code, mainly revolving
-- around the fact that version numbers should never contain
-- metacharacters.  In the Debian archive, this is always a safe
-- assumption; if it becomes not a safe assumption, the various
-- debugging statements that aptitude prints will have to be reworked
-- to unambiguously print package and version information.
module Resolver.Parse(
                      versionWithoutTerminators,
                      version,
                      packageWithoutTerminators,
                      package,
                      promotion,
                      dep,
                      choice,
                      solution
                     )
where

import Control.Monad(unless, when)

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.Parsec hiding(choice)
import Text.Parsec.ByteString(Parser)

import Resolver.Types

import qualified Data.ByteString.Char8 as BS

import Data.List(foldl')
import Data.Maybe(maybeToList)
import qualified Data.Map as Map(fromList)
import qualified Data.Set as Set(fromList)

seqList :: [a] -> ()
seqList lst = foldl' (flip seq) () lst

lexeme :: Parser t -> Parser t
lexeme p = do rval <- p
              spaces
              return rval

symbol :: String -> Parser String
symbol = lexeme . string

leftParen = symbol "("
rightParen = symbol ")"
leftAngle = symbol "<"
rightAngle = symbol ">"
leftSquare = symbol "["
rightSquare = symbol "]"
leftBrace = symbol "{"
rightBrace = symbol "}"
comma = symbol ","
semicolon = symbol ";"

parens = between leftParen rightParen
angles = between leftAngle rightAngle
squares = between leftSquare rightSquare
braces = between leftBrace rightBrace


-- | Alternation that throws away the actual parse.
a <||> b = (a >> return ()) <|> (b >> return ())

readCharsTill :: [Parser t] -> Parser String
readCharsTill terminators =
    ((eof <||> Parsec.choice (map lookAhead terminators) <||> space) >> return []) <|>
    do c <- anyChar
       rest <- readCharsTill terminators
       return (c:rest)

-- | A version is any non-whitespace string not containing one of the
-- given terminators.
versionWithoutTerminators :: [Parser t] -> Parser Version
versionWithoutTerminators terminators =
    (lexeme $ do p <- packageWithoutTerminators terminators
                 v <- lexeme $ readCharsTill terminators
                 when (null v) (fail "Missing version number.")
                 let vn = BS.pack v
                 p `seq` vn `seq` return $ Version { verPkg = p, verName = vn }) <?> "version"

version :: Parser Version
version = versionWithoutTerminators []


-- | A package is also any non-whitespace string.
packageWithoutTerminators :: [Parser t] -> Parser Package
packageWithoutTerminators terminators =
    (do w <- lexeme $ readCharsTill terminators
        when (null w) (fail "Missing package name.")
        let p = Package $ BS.pack w
        p `seq` return p) <?> "package name"

package :: Parser Package
package = packageWithoutTerminators []

-- | A dependency looks like this: version -> { ver ver ver }
depWithoutTerminators :: [Parser t] -> Parser Dep
depWithoutTerminators terminators =
    (do let term'  = [t >> return () | t <- terminators]
            arrow  = symbol "->"
        source <- versionWithoutTerminators $ (try arrow >> return ()):term'
        arrow
        leftBrace
        -- Throw away the input terminators and just use } instead,
        -- since we have a balanced group.  e.g., this means that in
        -- [a -> {b [UNINST]}] everything parses correctly.
        solvers <- lexeme $ manyTill (versionWithoutTerminators [rightBrace]) (try rightBrace)
        source `seq` seqList solvers `seq` return $ Dep { depSource = source, depSolvers = solvers }) <?> "dependency"

dep :: Parser Dep
dep = depWithoutTerminators []

-- | A choice is either Install(ver) or Break(dep)
choice :: Parser Choice
choice = (lexeme $ do installChoice <|> breakDepChoice) <?> "choice"
    where installChoice = do try (symbol "Install")
                             parens $ do
                               v <- versionWithoutTerminators [rightParen, leftSquare]
                               v `seq` ((do d <- squares dep
                                            let fromDepSource = Just True
                                                reason        = Just d
                                            (d `seq` fromDepSource `seq` reason `seq`
                                             (return $ InstallVersion { choiceVer = v,
                                                                        choiceVerReason = reason,
                                                                        choiceFromDepSource = fromDepSource }))) <|>
                                        (return $ InstallVersion { choiceVer = v,
                                                                   choiceVerReason = Nothing,
                                                                   choiceFromDepSource = Nothing }))
          breakDepChoice = do try (symbol "Break")
                              d <- parens dep
                              d `seq` return $ BreakSoftDep { choiceDep = d }

-- | Hacky non-lexeme parser for integers.
--
-- Uses the Read instance of Integer to do the actual parsing.
integer :: Parser Integer
integer = (do sign <- optionMaybe $ oneOf "-+"
              digits <- many1 digit
              return (read (maybeToList sign ++ digits))) <?> "integer"

-- | A solution looks like this:
--
-- < pkg1 := ver1 , pkg2 := ver2 , ... > ;
-- <! unresolved-soft-dep1 , unresolved-soft-dep2 , ... !> ;
-- [ broken-dep1 , broken-dep2 , ... ] ;
-- !! forbidden-ver1 , forbidden-ver2 , ... !!;
-- T<tier>S<score>
solution :: Parser Solution
solution = do installVersionChoices <- solutionInstalls
              unresolvedSoftDeps <- solutionUnresolvedSoftDeps
              brokenDeps <- solutionBrokenDeps
              forbiddenVers <- solutionForbiddenVers
              char 'T'
              tier <- integer
              char 'S'
              score <- integer
              let choices   = Map.fromList [(c, Nothing) | c <- installVersionChoices ++ unresolvedSoftDeps]
                  forbidden = Set.fromList forbiddenVers
                  broken    = Set.fromList brokenDeps
              choices `seq` forbidden `seq` broken `seq` score `seq` tier `seq`
                      return $ Solution { solChoices = choices,
                                          solForbiddenVersions = forbidden,
                                          solBrokenDeps = broken,
                                          solScore = score,
                                          solTier = Tier tier }
    where solutionInstalls =
              do bindings <- angles $ sepBy binding comma
                 spaces
                 semicolon
                 return bindings
          binding =
              choice <|>
              do p <- try $ packageWithoutTerminators [try $ symbol ":=", symbol ">"]
                 symbol ":="
                 v <- lexeme $ readCharsTill [comma, rightAngle]
                 return $ InstallVersion { choiceVer = Version { verPkg = p,
                                                                 verName = BS.pack v },
                                           choiceVerReason = Nothing,
                                           choiceFromDepSource = Nothing }
          solutionUnresolvedSoftDeps =
              do spaces
                 (do try (symbol "<!")
                     deps <- sepBy (try $ depWithoutTerminators [try $ symbol "!>"]) comma
                     symbol "!>"
                     semicolon
                     return [BreakSoftDep d | d <- deps])
                 <|> return []
          solutionBrokenDeps =
              do deps <- squares $ sepBy (try $ depWithoutTerminators [symbol "]"]) comma
                 semicolon
                 return deps
          solutionForbiddenVers =
              do (do try (symbol "!!")
                     vers <- sepBy (try $ versionWithoutTerminators [try $ symbol "!!", comma]) (comma)
                     symbol "!!"
                     semicolon
                     return vers) <|> return []

-- | A promotion looks like this: (T<tier>: {choice, ..})
promotion :: Parser Promotion
promotion = parens $ do char 'T'
                        tier <- lexeme integer
                        symbol ":"
                        choices <- braces $ sepBy choice comma
                        let choiceSet = Set.fromList choices
                        tier `seq` choiceSet `seq`
                             return $ Promotion { promotionTier = Tier tier,
                                                  promotionChoices = Set.fromList choices }
