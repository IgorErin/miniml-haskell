{
module Parser where

import Lexer as L
import Parsetree as P

import Data.List.NonEmpty hiding (reverse)
}

%name run
%tokentype  { L.Token }
%error      { parseError }

%token
   "fun"   { L.TFun }

   "("      { L.TLParent }
   ")"      { L.TRParent }

   "->"     { L.TArrow }

   "true"   { L.TTrue }
   "false"  { L.TFalse }
   "()"     { L.TUnit }

   "if"     { L.TIf }
   "then"   { L.TThen }
   "else"   { L.TElse }

   "bool"   { L.TBoolType }
   "unit"   { L.TUnitType }
   "int"    { L.TIntType }

   ":"      { L.TColon }

   "rec"    { L.TRec }
   "let"    { L.TLet }
   "in"     { L.TIn }
   "="      { L.TEq }

   "&"      { L.TBorrow }
   "mut"    { L.TMut }
   "local"  { L.TLocal }
   "once"   { L.TOnce }

   "_"      { L.TAny }

   int      { L.TInt $$ }

   ident    { L.TIdent $$ }
%%

Program :: { P.Expr }
Program : Expr                                { $1 }

Expr :: { P.Expr }
Expr
    : Expr AtomExpr                                { P.app $1 $2 }
    | AtomExpr                                     { $1 }

AtomExpr :: { P.Expr }
AtomExpr
    : Const                                         { $1 }
    | ident                                         { P.var $1 }
    | "(" Expr ")"                                  { $2 }
    | "&" AtomExpr                                  { P.borrow $2 }
    | "if" Expr "then" Expr "else" Expr             { P.if_ $2 $4 $6}
    | "fun" Pattern "->" Expr                       { P.lam $2 $4 }
    | "let" is("rec") Pattern list(Pattern) "=" Expr "in" Expr    { P.let_ $2 $3 $4 $6 $8 }

Const :: { P.Expr }
Const
    : int                                       { P.intConst $1 }
    | "true"                                    { P.boolConst True }
    | "false"                                   { P.boolConst False }
    | "()"                                      { P.unitConst}

Pattern :: { P.Pattern }
Pattern
    : PatternModifier ident                            { P.pvar $1 $2 }
    | "(" PatternModifier ident ":" TypeExpr ")"       { P.pascr $2 $3 $5 }
    | "()"                                      { P.punit }
    | "_"                                       { P.pany }
    | "(" Pattern ")"                           { $2 }

TypeExpr :: { P.Ty }
TypeExpr
    : "int"                                     { P.tint }
    | "bool"                                    { P.tbool }
    | "unit"                                    { P.tunit }
    | TypeExpr "->" TypeExpr                    { P.tarrow $1 $3 }
    | TypeExpr TypeExpr                         { P.tapp $1 $2 }

PatternModifier :: { P.PatternModifier }
PatternModifier
    : "mut"                                     { P.pmUnique }
    | "local"                                   { P.pmLocal }
    | "once"                                    { P.pmOnce }
    | {- empty -}                               { P.pmNone }

----------------------------- New helpers -----------------

fst(p, q)        : p q                 { $1 }
snd(p, q)        : p q                 { $2 }
both(p, q)       : p q                 { ($1,$2) }

opt(p)          : p                   { Just $1 }
                |                     { Nothing }

is(p)           : p                   { True }
                | {- empty -}         { False }

list(p) : rev_list(p)                 { reverse $1 }

rev_list(p)
    : rev_list(p) p                       { $2 : $1 }
    | {- empty -}                     { [] }

non_empty_list(p) : p list(p)         { $1 :| $2 }

sep(p, s)
    : p list(snd(s, p))               { $1 : $2 }
    | {- empty -}                     { [] }

{
parseError :: [L.Token] -> a
parseError _ = error "Parse error"
}
