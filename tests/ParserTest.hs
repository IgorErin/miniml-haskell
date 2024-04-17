{-# LANGUAGE TupleSections #-}

module ParserTest (tests) where

-- import Data.List.NonEmpty (NonEmpty ((:|)))
-- import qualified Data.List.NonEmpty as NonEmpty
-- import Data.Text (Text, unpack)

import Debug.Trace
import Parser (parseExpr)
import Parsetree
import Test.HUnit
import Utils (processTillParser)

exprBinOp f a b = EApp (EApp f a) b

(==?=>) :: String -> Maybe Program -> Assertion
(==?=>) text maybeAst = assertEqual ("[" <> text <> "]") maybeAst (processTillParser text)

testExpr :: String -> Either String Expr -> Assertion
testExpr text ast =
  case (ast, Parser.parseExpr text) of
    (Right exp, Right real) -> assertEqual ("[" <> text <> "]") exp real
    (Left a, Left b) -> assertEqual ("[" <> text <> "]") a b
    (Right left, Left err) -> trace err $ assertFailure "FUCK"
    (Right left, Left err) -> trace err $ assertFailure "FUCK"

-- Tests

testLetDecls :: Assertion
testLetDecls = do
  let varDecl' x v = SItem NonRecursive (PVar x) (econst_int v)

  let varDecl x = varDecl' x 4
  let funDecl x args = SItem NonRecursive (PVar x) $ elams args (econst_int 4)
  let recFunDecl x args = SItem Recursive (PVar x) $ elams args (econst_int 4)

  let aDecl = varDecl "a"
  let bDecl = varDecl' "b" 8
  let (@@) = EApp

  testExpr "a" (Right $ EVar "a")
  testExpr "a+b" (Right $ exprBinOp (EVar "+") (EVar "a") (EVar "b"))
  testExpr "let x = 1 in x" (Right $ ELet NonRecursive (PVar "x") (econst_int 1) (EVar "x"))
  testExpr
    "let rec fix f = f (fix f) x in fix"
    ( Right $
        ELet
          Recursive
          (PVar "fix")
          ( ELam
              (PVar "f")
              ((EVar "f" @@ (EVar "fix" @@ EVar "f")) @@ EVar "x")
          )
          (EVar "fix")
    )

  "let a = 4 " ==?=> Just (Program [aDecl])

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
