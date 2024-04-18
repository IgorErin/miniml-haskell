{-# LANGUAGE QuasiQuotes #-}

module ParserTest (tests) where

import Debug.Trace
import Parser (parseExpr, parseProgram)
import Parsetree
import Test.HUnit
import Text.RawString.QQ

-- import Utils (processTillParser)

exprBinOp f a = EApp (EApp f a)

(==?=>) :: String -> Either String Program -> Assertion
(==?=>) text ast =
  case (ast, Parser.parseProgram text) of
    (Right exp, Right real) -> assertEqual ("[" <> text <> "]") exp real
    (Left a, Left b) -> assertEqual ("[" <> text <> "]") a b
    (Right left, Left err) -> trace err $ assertFailure "FUCK"
    (Right left, Left err) -> trace err $ assertFailure "FUCK"

programParsable :: String -> Assertion
programParsable text =
  case Parser.parseProgram text of
    Right _ -> assertEqual "" True True
    Left err -> trace err $ assertFailure ("Can't parse: " ++ text)

-- assertEqual
-- ("[" <> text <> "]")
-- maybeAst
-- (processTillParser text)

testExpr :: String -> Either String Expr -> Assertion
testExpr text ast =
  case (ast, Parser.parseExpr text) of
    (Right exp, Right real) -> assertEqual ("[" <> text <> "]") exp real
    (Left a, Left b) -> assertEqual ("[" <> text <> "]") a b
    (Right left, Left err) -> trace err $ assertFailure "FUCK"
    (Right left, Left err) -> trace err $ assertFailure "FUCK"

-- Tests

testLetDecls :: Assertion
testLetDecls =
  do
    let varDecl' x v = SItem NonRecursive (pvar x) (econst_int v)

    let varDecl x = varDecl' x 4
    let funDecl x args = SItem NonRecursive (pvar x) $ elams args (econst_int 4)
    let recFunDecl x args = SItem Recursive (pvar x) $ elams args (econst_int 4)

    let aDecl = varDecl "a"
    let bDecl = varDecl' "b" 8
    let (@@) = EApp

    testExpr "a" (Right $ EVar "a")
    testExpr "a+b" (Right $ exprBinOp (EVar "+") (EVar "a") (EVar "b"))
    testExpr "let x = 1 in x" (Right $ ELet NonRecursive (pvar "x") (econst_int 1) (EVar "x"))
    testExpr
      "let rec fix f = f (fix f) x in fix"
      ( Right $
          ELet
            Recursive
            (pvar "fix")
            ( ELam
                (pvar "f")
                ((EVar "f" @@ (EVar "fix" @@ EVar "f")) @@ EVar "x")
            )
            (EVar "fix")
      )

    "let a = 4 " ==?=> Right (Program [aDecl])
    "let a = let () = print_int 5 in 42"
      ==?=> Right
        ( Program
            [ SItem
                NonRecursive
                (pvar "a")
                ( ELet
                    NonRecursive
                    PUnit
                    ( EApp
                        (EVar "print_int")
                        (EConst (PConst_int 5))
                    )
                    (EConst (PConst_int 42))
                )
            ]
        )
    programParsable
      "let print_and_close (  file) =\n\
      \let () = print_endline (read file) in\n\
      \close file"
    "let f (unique q) = q"
      ==?=> Right (Program [SItem NonRecursive (PVar "f" PMNone) (ELam (PVar "q" PMUnique) (EVar "q"))])

-- "let a" ==?=> Nothing
-- "let = 4" ==?=> Nothing
-- "leta = 4" ==?=> Just (Program [StmtExpr (exprBinOp (CompOp EqOp) (ExprId "leta") (EConst (PrimValInt 4)))])

-- "let rec a = 4" ==?=> Nothing
-- "let rec a b = 4" ==?=> Just (Program [recFunDecl "a" ["b"]])
-- "let reca a b = 4" ==?=> Just (Program [funDecl "reca" ["a", "b"]])
-- "leta rec a b = 4" ==?=> Nothing
-- "letaa rec a b = 4" ==?=> Nothing
-- "let reca = 4" ==?=> Just (Program [varDecl "reca"])
-- "let recaa = 4" ==?=> Just (Program [varDecl "recaa"])
-- "let let a = 4" ==?=> Nothing
-- "let let = 4" ==?=> Nothing
-- "let leta = 4" ==?=> Just (Program [varDecl "leta"])

-- "let a = 4 let b = 8" ==?=> Just (Program [aDecl, bDecl])
-- "let a = 4\nlet b = 8" ==?=> Just (Program [aDecl, bDecl])
-- "let a = 4;;let b = 8" ==?=> Just (Program [aDecl, bDecl])
-- ";;let a = 4;;;;let b = 8;;;;;; ;;" ==?=> Just (Program [aDecl, bDecl])

-- testWhitespace :: TestTree
-- testWhitespace = testCase "whitespace" $ do
--   let decl =
--         Just
--           ( Program
--               [ StmtDecl
--                   ( DeclFun
--                       "f"
--                       False
--                       ( Fun
--                           (("a", Nothing) :| [])
--                           Nothing
--                           ( exprBinOp
--                               (ArithOp MulOp)
--                               (ExprId "a")
--                               ( EApp
--                                   (EApp (ExprId "a") (ExprId "f"))
--                                   (EConst (PrimValInt 4))
--                               )
--                           )
--                       )
--                   )
--               ]
--           )
--   let declAndApp =
--         Just
--           ( Program
--               [ StmtDecl
--                   ( DeclFun
--                       "f"
--                       False
--                       ( Fun
--                           (("a", Nothing) :| [])
--                           Nothing
--                           (exprBinOp (ArithOp MulOp) (ExprId "a") (ExprId "a"))
--                       )
--                   ),
--                 StmtExpr (EApp (ExprId "f") (EConst (PrimValInt 4)))
--               ]
--           )

--   "let f a = a * a f 4" ==?=> decl
--   "let f a = a * a\nf 4" ==?=> decl
--   "let f a = a * a;;f 4" ==?=> declAndApp

-- TODO : abs_max

-- testUnaryMinusOp :: TestTree
-- testUnaryMinusOp = testCase "unary minus operator" $ do
--   let prgStmtExpr e = Just (Program [StmtExpr e])

--   let zero = econst_int 0
--   let seven = econst_int 7
--   let a = EVar "a"
--   let b = EVar "b"

--   let minus = (EVar "-")

--   "-7" ==?=> prgStmtExpr (minus seven)
--   "- 7" ==?=> prgStmtExpr (minus seven)
--   "0 - 7"
--     ==?=> prgStmtExpr
--       (exprBinOp (EVar "-") zero seven)
--       "a - 7"
--     ==?=> prgStmtExpr (exprBinOp (EVar "-") a seven)
--   "a - b" ==?=> prgStmtExpr (exprBinOp (EVar "-") a b)
--   "a - -b" ==?=> prgStmtExpr (exprBinOp (EVar "-") a (minus b))

tests :: Test
tests =
  TestList
    [ TestCase testLetDecls
    -- testWhitespace,
    -- testUnaryMinusOp
    ]
