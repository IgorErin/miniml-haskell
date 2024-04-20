
{
module Lexer(alexScanTokens, Token (..)) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

@ident = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*
@number = digit+

tokens :-
  $white+               ;
  $digit+               { toInt }

  ":"                   { const TColon }
  "->"                  { const TArrow }
  "="                   { const TEq }
  "("                   { const TLParent }
  ")"                   { const TRParent }
  "_"                   { const TAny }

  "let"                 { const TLet }
  "rec"                 { const TRec }
  "in"                  { const TIn }

  "if"                  { const TIf }
  "then"                { const TThen }
  "else"                { const TElse }

  "fun"                 { const TFun }

  "true"                { const TTrue }
  "false"               { const TFalse }
  "()"                  { const TUnit }

  "unit"                { const TUnitType }
  "bool"                { const TBoolType }
  "int"                 { const TIntType }

  "&"                   { const TBorrow }

  "mut"                 { const TMut }
  "local"               { const TLocal }
  "once"                { const TOnce }

  @ident                { TIdent }
{
toInt :: String -> Token
toInt = TInt . read

data Token
  =
  TLParent
  | TRParent
  | TColon
  | TEq
  | TLet
  | TIn
  | TRec
  | TAny

  | TIf
  | TThen
  | TElse

  | TTrue
  | TFalse
  | TUnit

  | TUnitType
  | TBoolType
  | TIntType

  | TBorrow

  | TMut
  | TLocal
  | TOnce

  | TFun
  | TArrow
  | TIdent String
  | TInt Int
  deriving (Eq, Show)
}
