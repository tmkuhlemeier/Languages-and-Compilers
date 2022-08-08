module Model where

-- Exercise 1
data Token = TArrow
           | TPeriod
           | TComma
           | TGo
           | TTake
           | TMark
           | TNothing
           | TTurn
           | TCase
           | TOf
           | TEnd
           | TLeft
           | TRight
           | TFront
           | TSemiColon
           | TEmpty
           | TLambda
           | TDebris
           | TAsteroid
           | TBoundary
           | TUnderscore
           | TIdentifier String

instance Show Token where
    show TArrow = "TArrow"
    show TPeriod = "TPeriod"
    show TComma = "TComma"
    show TGo = "TGo"
    show TTake = "TTake"
    show TMark = "TMark"
    show TNothing = "TNothing"
    show TTurn = "TTurn"
    show TCase = "TCase"
    show TOf = "TOf"
    show TEnd = "TEnd"
    show TLeft = "TLeft"
    show TRight = "TRight"
    show TFront = "TFront"
    show TSemiColon = "TSemiColon"
    show TEmpty = "TEmpty"
    show TLambda = "TLambda"
    show TDebris = "TDebris"
    show TAsteroid = "TAsteroid"
    show TBoundary = "TBoundary"
    show TUnderscore = "TUnderscore"
    show (TIdentifier s) = "(TIdentifier \"" ++ s ++ "\")"

-- Exercise 2

-- Program is a list of rules
data Program = PrProgram [Rule] deriving Show

-- Rule is an Identifier with a list of Commands
data Rule = RRule Id [Cmd]

-- An Identifier is a String
type Id = String

-- The Commands Consist of Go, Take, Mark, Nothing, Turn, CaseOf, and Identifier
-- Turn takes a Direction and Caseof a Direction and a list of Alts
data Cmd = CGo | CTake | CMark | CNothing | CTurn Dir
         | CCaseOf Dir [Alt] | CId Id

-- The three directions you can turn in
data Dir = DLeft | DRight | DFront
    deriving Eq

-- An Alt has a Pattern and a list of Commands that belong to that Pattern
data Alt = AAlt Pat [Cmd]

-- The Patterns that are available
data Pat = PtEmpty | PtLambda | PtDebris | PtAsteroid
         | PtBoundary | PtUnderscore
    deriving Eq

-- Show instances:

instance Show Rule where
    show (RRule x y) = "Rule ((Identifier (" ++ x ++ ")) (" ++ show y ++ "))"

instance Show Dir where
    show DLeft = "Dir (left)"
    show DRight = "Dir (right)"
    show DFront = "Dir (front)"

instance Show Alt where
    show (AAlt x y) = "Alt (" ++ show x ++ " (" ++ show y ++ "))"

instance Show Pat where
    show PtEmpty = "Pat (Empty)"
    show PtLambda = "Pat (Lambda)"
    show PtDebris = "Pat (Debris)"
    show PtAsteroid = "Pat (Asteroid)"
    show PtBoundary = "Pat (Boundary)"
    show PtUnderscore = "Pat (_)"

instance Show Cmd where
    show CGo = "Cmd (go)"
    show CTake = "Cmd (take)"
    show CMark = "Cmd (mark)"
    show CNothing = "Cmd (nothing)"
    show (CTurn x) = "Cmd (turn " ++ show x ++ ")"
    show (CCaseOf x y) = "Cmd (case " ++ show x ++ " of (" ++ show y ++ "))"
    show (CId x) = "Cmd (Identifier (" ++ x ++ "))"
