module Model where

-- Exercise 1
data Token = Token
           | TArrow
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
           | TIdent String

instance Show Token where
    show Token = "Token"
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
    show TIdent s = "TIdent:" ++ s

-- Exercise 2
data Program = Program [Rule]

data Rule = Rule Iden Cmds | Rule1 Iden

data Iden = Iden Symb [Symb]
data Symb = Letter | Digit | Plus | Minus

data Cmds = Cmds Cmd [Cmd]
          | Cmds1 Cmd

data Cmd = CGo | CTake | CMark | CNothing | CTurn Dir
         | CCaseOf Dir Alts | CIden Iden

data Dir = Left | Right | Front

data Alts = Alts Alt

data Alt = Alt Pat Cmds

data Pat = PEmpty | PLambda | PDebris | PAsteroid
         | PBoundary | PUnderscore
