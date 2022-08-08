{
module Parser where

import Model
}

%name parseP
%tokentype { Token }

%token
  '->'                { TArrow }
  '.'                 { TPeriod }
  ','                 { TComma }
  go                  { TGo }
  take                { TTake }
  mark                { TMark }
  nothing             { TNothing }
  turn                { TTurn }
  case                { TCase }
  of                  { TOf }
  end                 { TEnd }
  left                { TLeft }
  right               { TRight }
  front               { TFront }
  ';'                 { TSemiColon }
  Empty               { TEmpty }
  Lambda              { TLambda }
  Debris              { TDebris }
  Asteroid            { TAsteroid }
  Boundary            { TBoundary }
  '_'                 { TUnderscore }
  identifier          { TIdentifier $$ }

%%

Program : Rules             { PrProgram $1 }

Rules : Rules1              { reverse $1 }

-- using left recursion for improved efficiency in Happy:
Rules1 : Rules1 Rule        { $2 : $1 }
       | {- empty -}        { [] }

Rule  : identifier '->' Cmds '.' { RRule $1 $3 }

Cmds  : Cmds1               { reverse $1 }
      | Cmd                 { [$1] }                     
      | {- empty -}         { [] }

-- using left recursion again:
Cmds1 : Cmds1 ',' Cmd       { $3 : $1 }
      | Cmd                 { [$1]    }

Cmd : go                    { CGo }
    | take                  { CTake }
    | mark                  { CMark }
    | nothing               { CNothing }
    | turn Dir              { CTurn $2 }
    | case Dir of Alts end  { CCaseOf $2 $4 }
    | identifier            { CId $1 }

Dir : left                  { DLeft }
    | right                 { DRight }
    | front                 { DFront }

Alts  : Alts1               { reverse $1 }
      | Alt                 { [$1] }                     
      | {- empty -}         { [] }

-- using left recursion again:
Alts1 : Alts1 ';' Alt       { $3 : $1 }
      | Alt                 { [$1]    }

Alt : Pat '->' Cmds         { AAlt $1 $3 }

Pat : Empty                 { PtEmpty }
    | Lambda                { PtLambda }
    | Debris                { PtDebris }
    | Asteroid              { PtAsteroid }
    | Boundary              { PtBoundary }
    | '_'                   { PtUnderscore }

{

happyError _ = error "parse error"

parseProgram :: [Token] -> Program
parseProgram tokens = parseP tokens
  

}