{
module Parser where

import Model
}

%name foo
%tokentype { Token }

%token
  '->' { TArrow }
  '.' { TPeriod }
  ',' { TComma }
  go { TGo }
  take { TTake }
  mark { TMark }
  nothing { TNothing }
  turn { TTurn }
  case { TCase }
  of { TOf }
  end { TEnd }
  left { TLeft }
  right { TRight }
  front { TFront }
  empty { TEmpty }
  lambda { TLambda }
  debris { TDebris }
  asteroid { TAsteroid }
  boundary { TBoundary }
  '_' { TUnderscore }
  ident { TIdent }

%%

Program : Rules { Program $1 }

Rules : Rule Rules { Rules ($1:$2) }

Rule : ident '->' Cmds '.' { Rule $1 $3 }
     | ident '->' '.'      { Rule $1 }

Cmds : Cmd ',' Cmds        { Cmds ($1:$3) }
     | Cmd                 { Cmds1 [$1] }

{

happyError _ = error "parse error"

}