{
module Lexer (main) where

import Model
}

%wrapper "basic"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
  $white+ ;
  "--".*	;
  "->" { \s -> TArrow }
  "." { \s -> TPeriod }
  "," { \s -> TComma }
  "go" { \s -> TGo }
  "take" { \s -> TTake }
  "mark" { \s -> TMark }
  "nothing" { \s -> TNothing }
  "turn" { \s -> TTurn }
  "case" { \s -> TCase }
  "of" { \s -> TOf }
  "end" { \s -> TEnd }
  "left" { \s -> TLeft }
  "right" { \s -> TRight }
  "front" { \s -> TFront }
  "Empty" { \s -> TEmpty }
  "Lambda" { \s -> TLambda }
  "Debris" { \s -> TDebris }
  "Asteroid" { \s -> TAsteroid }
  "Boundary" { \s -> TBoundary }
  "_" { \s -> TUnderscore }
  [$alpha $digit \+ \-]+ { \s -> TIdent s }

{
main = do
   print (alexScanTokens ""fgfcb sfdvnjk dbvnj...)
}