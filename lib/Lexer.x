
{
module Lexer(alexScanTokens, Token (..)) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$backSlash = \

@ident = $alpha [$alpha $digit]*
@number = digit+

tokens :-
  $white+               ;
  $digit+               { toInt }

  ":"                   { const TColon }
  "->"                  { const TArrow }
  "="                   { const TEq }
  "("                   { const TLParent }
  ")"                   { const TRParent }

  "let"                 { const TLet }
  "rec"                 { const TRec }
  "in"                  { const TIn }

  "if"                  { const TIf }
  "then"                { const TThen }
  "else"                { const TElse }

  "fun"                 { const TFun }

  "true"                { const TTrue }
  "false"               { const TFalse }

  "unit"                { const TUnitType }
  "bool"                { const TBoolType }
  "int"                 { const TIntType }

  @ident                { TIdent }

{
toInt :: String -> Token
toInt = TInt . read

data Token
  =
  -- ( )
  TLParent
  | TRParent
  | TColon
  | TEq
  | TLet
  | TIn
  | TRec

  -- if then else
  | TIf
  | TThen
  | TElse

  | TTrue
  | TFalse

  | TUnitType
  | TBoolType
  | TIntType
  
  | TFun
  | TArrow
  | TIdent String
  | TInt Int
  deriving (Eq, Show)
}
