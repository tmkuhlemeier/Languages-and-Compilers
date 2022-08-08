{
module Lexer (module Lexer) where

import Model
import Parser
}
-- Unfortunately, we could not find a way to correctly lex
-- strings containing keywords within such as "return". This resulted
-- namely in CId "re" and CTurn dir
-- We experimented a lot and did a lot of research, but to no avail
-- It should lex correctly however when Ids do not contain keywords.

%wrapper "basic"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

$alphaNum = [a-zA-Z0-9]

tokens :-
  $white+                 ;
  "--".*	                ;
  "->"                    { \s -> TArrow }
  "."                     { \s -> TPeriod }
  ","                     { \s -> TComma }
  "go"                    { \s -> TGo }
  "take"                  { \s -> TTake }
  "mark"                  { \s -> TMark }
  "nothing"               { \s -> TNothing }
  "turn"                  { \s -> TTurn }
  "case"                  { \s -> TCase }
  "of"                    { \s -> TOf }
  "end"                   { \s -> TEnd }
  "left"                  { \s -> TLeft }
  "right"                 { \s -> TRight }
  "front"                 { \s -> TFront }
  ";"                     { \s -> TSemiColon }
  "Empty"                 { \s -> TEmpty }
  "Lambda"                { \s -> TLambda }
  "Debris"                { \s -> TDebris }
  "Asteroid"              { \s -> TAsteroid }
  "Boundary"              { \s -> TBoundary }
  "_"                     { \s -> TUnderscore }
  [$alpha $digit \+ \-]+  { \s -> TIdentifier s }

{
main :: String -> [Token]
main input = alexScanTokens input

lexProgram :: String -> [Token]
lexProgram input = alexScanTokens input

}