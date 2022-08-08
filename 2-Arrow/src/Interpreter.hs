{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interpreter where

import ParseLib.Abstract
import Prelude hiding ((<*), (<$))

import Data.Map (Map)
import qualified Data.Map as L

import Data.Char (isSpace)
import Control.Monad (replicateM)
import Data.Maybe

import Lexer
import Parser
import Model
import Algebra
import qualified Lexer


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary deriving Eq

instance Show Contents where
  show Empty = "."
  show Lambda = "\\"
  show Debris = "%"
  show Asteroid = "O"
  show Boundary = "#"

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents

-- | Parses a space file that can be found in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces


-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )]

-- Exercise 7

-- Prints the space to the console
printSpace :: Space -> String
printSpace spaceMap =   show pos ++ "\n"
                    ++  stackRows spaceMap (0,0) x y
  where pos@(x,y) = fst $ L.findMax spaceMap

-- Prints all rows and stacks them on top of each other
stackRows :: Space -> Pos -> Int -> Int -> String
stackRows spaceMap pos@(y,_) maxY maxX  | y < maxY    = printRow spaceMap (y,0) maxX ++ "\n" ++ stackRows spaceMap (y+1,0) maxY maxX
                                          | y == maxY   = printRow spaceMap (y,0) maxX ++ stackRows spaceMap (y+1,0) maxY maxX
                                          | otherwise   = ""

-- Prints a single row
printRow :: Space -> Pos -> Int -> String
printRow spaceMap pos@(y,x) max | x <= max   = printPosition spaceMap pos ++ printRow spaceMap (y,x+1) max
                                | otherwise  = ""

-- Prints a single field
printPosition :: Space -> Pos -> String
printPosition spaceMap pos@(y,x) = show $ fromJust $ L.lookup pos spaceMap

-- These three should be defined by you
type Ident = String
type Commands = [Cmd]
data Heading = North | East | South | West deriving Eq

-- The environment takes Idents=Strings as keys and Commands=[Cmd] as values
type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

instance Show Step where
  show (Done x y z) = "done"
  show (Ok x)       = "Ok"
  show (Fail x)     = x

-- | Exercise 8

-- Parses a program from a certain string, checks the program if it is valid,
-- and if so, makes an environment out of it
toEnvironment :: String -> Environment
toEnvironment input | checkProgram program = programToEnvironment program L.empty
                    | otherwise = L.empty
  where program     = Parser.parseP $ Lexer.lexProgram input

-- Recursively iterates over the rules of a program to make an Environment out of it
programToEnvironment :: Program -> Environment -> Environment
programToEnvironment (PrProgram []) env = env
programToEnvironment (PrProgram ((RRule id cmds):xs)) env =
 programToEnvironment (PrProgram xs) (L.insert id cmds env)

-- | Exercise 9

-- Pattern matches on the type of command that is the first command of the stack and performs the right action accordingly
-- If the stack is empty, it returns the Done end position
step :: Environment -> ArrowState -> Step
step _ (ArrowState space pos heading []) = Done space pos heading
step env arr@(ArrowState space pos heading (c:cs)) = execCmd c (ArrowState space pos heading cs)
  where execCmd (CGo) arr               = goStep arr
        execCmd (CTake) arr             = takeStep arr
        execCmd (CMark) arr             = markStep arr
        execCmd (CNothing) arr          = Ok arr
        execCmd (CTurn dir) arr         = turnStep dir arr
        execCmd (CCaseOf dir alts) arr  = caseStep dir alts env arr
        execCmd (CId x) arr             = callStep x env arr

-- The go command checks the space it wants to move to, and moves to it if the space is free
-- If the space is out of bounds or blocked, it shows this
goStep :: ArrowState -> Step
goStep ar@(ArrowState space pos@(y,x) hdir (cmd:cmds))  | isJust mbcontent = testContent (fromJust mbcontent) (ArrowState space (calculatePosition pos 1 hdir) hdir cmds)
                                                        | otherwise        = Fail "Out of bounds"
  where mbcontent = L.lookup (calculatePosition pos 1 hdir) space

-- Tests whether the space is blocked or free
testContent :: Contents -> ArrowState -> Step
testContent content ar | content == Empty  ||
                         content == Lambda ||
                         content == Debris   = Ok ar
                       | otherwise           = Fail "Blocked"

-- Calculates a new position relative to a given position, based on the Heading and the number of spaces you want to jump
calculatePosition :: Pos -> Int -> Heading -> Pos
calculatePosition pos@(y,x) z hdir
  | hdir == North = (y-z,x)
  | hdir == East  = (y,x+z)
  | hdir == South = (y+z,x)
  | hdir == West  = (y,x-z)

-- Changes a space from one content to another
changeSpace :: (Contents -> Contents) -> ArrowState -> ArrowState
changeSpace f (ArrowState space pos heading stack) = ArrowState (L.adjust f pos space) pos heading stack

-- Take command that changes the current position of the arrow to Empty
takeStep :: ArrowState -> Step
takeStep arrowstate = Ok (changeSpace (const Empty) arrowstate)

-- Mark command that changes the current position to a Lambda
markStep :: ArrowState -> Step
markStep arrowstate = Ok (changeSpace (const Lambda) arrowstate)

-- Nothing command that does nothing
nothingStep :: ArrowState -> Step
nothingStep = Ok

-- Turn command that turns the arrow in a new direction
turnStep :: Dir -> ArrowState -> Step
turnStep dir (ArrowState space pos heading stack) = Ok (ArrowState space pos (turn dir heading) stack)

-- Checks which direction the arrow has to turn
turn:: Dir -> Heading -> Heading
turn DRight = turnRight
turn DLeft = turnLeft
turn _ = id

-- Turns the position left relative to its heading
turnLeft :: Heading -> Heading
turnLeft heading | heading == North = West
                 | heading == East = North
                 | heading == South = East
                 | heading == West = South

-- Turns the position right relative to its heading
turnRight :: Heading -> Heading
turnRight heading | heading == North = East
                  | heading == East = South
                  | heading == South = West
                  | heading == West = North

-- Uses direction, alternatives, environment and arrowstate to get a step
-- First the pattern in the position is returned by sensorRead
-- This contents pattern is translated to constructor of Pat datatype (since these are named differently)
-- Then, this pattern constructor is used to find a match in alts with matchAlt
-- The result of matchAlt is then transformed by stackUpdate to a
-- fail when Nothing, and else into an OK Step with updated stack
caseStep :: Dir -> [Alt] -> Environment -> ArrowState -> Step
caseStep dir alts env arr@(ArrowState space pos heading stack) = stackUpdate (matchAlt (patTranslate pattern) alts)
  where pattern = sensorRead dir arr
        stackUpdate (Nothing) = Fail ("No alternative match for "++show(pattern))
        stackUpdate (Just cmds) = Ok (ArrowState space pos heading (cmds ++ stack))

--pattern mapping from contents constructor to Pat constructor
patTranslate :: Contents -> Pat
patTranslate Empty = PtEmpty
patTranslate Lambda = PtLambda
patTranslate Debris = PtDebris
patTranslate Asteroid = PtAsteroid
patTranslate Boundary = PtBoundary

--Match returns list of commands if patterns match or if underscore, else look further
--If not found or empty, return Nothing
matchAlt :: Pat -> [Alt] -> Maybe [Cmd]
matchAlt _ [] = Nothing
matchAlt pt ((AAlt pattern cmds):alts) | pt == pattern           = Just cmds
                                       | pattern == PtUnderscore = Just cmds
                                       | otherwise               = matchAlt pt alts

-- Reads the Content pattern value from the position in the specified direction
-- Uses helper sense which looks up the contents in the direction in space
-- readPattern then translate a no contents found (Nothing) to Boundary
-- and all Just pattern to pattern
sensorRead :: Dir -> ArrowState -> Contents
sensorRead dir (ArrowState space pos heading _) = readPattern (sense (turn dir heading) pos space)
  where readPattern (Nothing) = Boundary
        readPattern (Just pattern) = pattern

-- Looks up the pattern in a heading in space
sense :: Heading -> Pos -> Space -> Maybe Contents
sense heading (x,y) space =  getPattern heading
  where getPattern North = L.lookup (x,y+1) space
        getPattern West  = L.lookup (x-1,y) space
        getPattern South = L.lookup (x,y-1) space
        getPattern East  = L.lookup (x+1,y) space

-- Looks up the commands from the rule that is called
-- stackUpdate then transforms a Nothing into a Fail Step, and a Just cmds
-- into an Ok Step with the commands prepended in the updated stack
callStep :: Id -> Environment -> ArrowState -> Step
callStep x env (ArrowState space pos heading stack) = stackUpdate (L.lookup x env)
  where stackUpdate (Nothing) = Fail (x ++ " is not defined")
        stackUpdate (Just cmds) = Ok (ArrowState space pos heading (cmds ++ stack))



