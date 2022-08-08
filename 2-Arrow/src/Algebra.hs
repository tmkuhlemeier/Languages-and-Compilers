module Algebra where

import Model
import Parser
import Data.Set hiding (fold,map)

-- Exercise 5
type Algebra pr r c d a pt = (
    [r] -> pr,      --program
    Id -> [c] -> r, --rule
    c,              --go
    c,              --take_
    c,              --mark
    c,              --nothing
    d -> c,         --turn
    d -> [a] -> c,  --caseof
    Id -> c,        --ident
    d,              --left
    d,              --right
    d,              --front
    pt -> [c] -> a, --alt
    pt,             --empty
    pt,             --lambda
    pt,             --debris
    pt,             --asteroid
    pt,             --boundary
    pt              --underscore
    )

-- Definition of fold, which applies functions from the algebra provided
fold :: Algebra pr r c d a pt -> Program -> pr
fold (program,rule,go,take_,mark,nothing,turn,caseof,
    ident,left,right,front,alt,empty,lambda,debris,
    asteroid,boundary,underscore) = fpr
    where fpr   (PrProgram rs) = program (map fr rs)
          fr    (RRule x cs) = rule x (map fc cs)
          fc    (CGo) = go
          fc    (CTake) = take_
          fc    (CMark) = mark
          fc    (CNothing) = nothing
          fc    (CTurn d) = turn (fd d)
          fc    (CCaseOf d as) = caseof (fd d) (map fa as)
          fc    (CId x) = ident x
          fd    (DLeft) = left
          fd    (DRight) = right
          fd    (DFront) = front
          fa    (AAlt p cs) = alt (fpt p) (map fc cs)
          fpt   (PtEmpty) = empty
          fpt   (PtLambda) = lambda
          fpt   (PtDebris) = debris
          fpt   (PtAsteroid) = asteroid
          fpt   (PtBoundary) = boundary
          fpt   (PtUnderscore) = underscore

-- Exercise 6

-- Does a series of sanity checks
-- Checks points 1, 2, and 3 with checkRules, and 4 with checkPatternMatch
checkProgram :: Program -> Bool
checkProgram p = checkRules rules calls && checkPatternMatch p
    where rules = fold ruleAlgebra p
          calls = fold callAlgebra p

-- Checks

-- 1, 2, and 3 combined
-- gets input lists of rules and calls
checkRules :: [String] -> [String] -> Bool
checkRules rules calls = checkStart rules && checkDefined rules calls && checkNoRuleDefinedTwice rules

-- 1: returns whether the list of rule ids contains a start
-- gets input list of rules
checkStart :: [String] -> Bool
checkStart = elem "start"

-- 2: returns whether the set of called rules a subset of defined rules
-- gets input lists of rules and calls
checkDefined :: Ord a => [a] -> [a] -> Bool
checkDefined rules calls = isSubsetOf (fromList calls) (fromList rules)

-- 3: returns whether the no rule is defined twice
-- gets input list of rules
checkNoRuleDefinedTwice :: [String] -> Bool
checkNoRuleDefinedTwice rules = length rules == (size (fromList rules))

-- 4: returns whether the patterns are correctly matched
-- gets input program
checkPatternMatch :: Program -> Bool
checkPatternMatch = fold patternMatchAlgebra

-- Algebras

-- This algebra is defined to propagate the ids of rules upwards
ruleAlgebra :: Algebra [Id] Id Cmd Dir Alt Pat
ruleAlgebra = (
    id,    -- return the list of rules
    const, -- first arg of rule is its id
    CGo,   -- rest of the functions simply return the original values
    CTake,
    CMark,
    CNothing,
    CTurn,
    CCaseOf,
    CId,
    DLeft,
    DRight,
    DFront,
    AAlt,
    PtEmpty,
    PtLambda,
    PtDebris,
    PtAsteroid,
    PtBoundary,
    PtUnderscore )

-- This algebra is defined to propagate the ids of commands (calls) upwards
-- Second arg of rule is list of commands, return a list of 
-- only the ids of CId in the list of commands
-- Program gets a list of lists of ids (called) concatenates all these lists into one
-- Rest of the functions simply return the original values
callAlgebra :: Algebra [Id] [Id] Cmd Dir Alt Pat
callAlgebra = (
    concat,                        -- program
    (\ _ cs -> [x | CId x <- cs]), -- rule
    CGo,
    CTake,
    CMark,
    CNothing,
    CTurn,
    CCaseOf,
    CId,
    DLeft,
    DRight,
    DFront,
    AAlt,
    PtEmpty,
    PtLambda,
    PtDebris,
    PtAsteroid,
    PtBoundary,
    PtUnderscore )

-- For each CaseOf Command in the program, it gets the list of Alts that are stored in there. This list is passed
-- on to a function that checks whether all patterns are present in the CaseOf, or otherwise at least the catch all underscore
patternMatchAlgebra :: Algebra Bool Bool Bool Bool Pat Pat
patternMatchAlgebra = ( and,
                        \_ cs -> and cs,
                        True,
                        True,
                        True,
                        True,
                        const True,
                        \_ x -> patternMatchCheck x,
                        const True,
                        True,
                        True,
                        True,
                        const,
                        PtEmpty,
                        PtLambda,
                        PtDebris,
                        PtAsteroid,
                        PtBoundary,
                        PtUnderscore )

-- Auxilary Functions Pattern Match Check

-- Checks whether all patterns, or if not at least the underscore is present in a list of patterns
patternMatchCheck :: [Pat] -> Bool
patternMatchCheck x =   PtEmpty `elem` x
                    &&  PtLambda `elem` x
                    &&  PtDebris `elem` x
                    &&  PtAsteroid `elem` x
                    &&  PtBoundary `elem` x
                    ||  PtUnderscore `elem` x

