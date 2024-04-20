-- {-# LANGUAGE QuasiQuotes #-}

module ParserTest () where

-- import Debug.Trace
-- import Parser
-- import Parsetree
-- import Test.HUnit

-- exprBinOp :: Expr -> Expr -> Expr -> Expr
-- exprBinOp f a = EApp (EApp f a)

-- (==?=>) :: String -> Either String Program -> Assertion
-- (==?=>) text ast =
--   case (ast, Parser.parseProgram text) of
--     (Right expected, Right real) -> assertEqual ("[" <> text <> "]") expected real
--     (Left a, Left b) -> assertEqual ("[" <> text <> "]") a b
--     (Right _, Left err) -> trace err $ assertFailure "FUCK"
--     (Left err, Right _) -> trace err $ assertFailure "FUCK"

-- wrapParsable :: String -> (String -> Either String a) -> String -> Assertion
-- wrapParsable name p text =
--   case p text of
--     Right _ -> assertEqual "" True True
--     Left err -> trace err $ assertFailure ("Can't parse " ++ name ++ " : " ++ text)

-- programParsable :: String -> Assertion
-- programParsable = wrapParsable "program" parseProgram
-- identParsable :: String -> Assertion
-- identParsable = wrapParsable "ident" parseIdent

-- patternParsable :: String -> Assertion
-- patternParsable text =
--   case Parser.parsePattern text of
--     Right _ -> assertEqual "" True True
--     Left err -> trace err $ assertFailure ("Can't parse: `" ++ text ++ "`")

-- tyParsable :: String -> Assertion
-- tyParsable = wrapParsable "ty" parseTy

-- testExpr :: String -> Expr -> Assertion
-- testExpr text ast =
--   case Parser.parseExpr text of
--     Right real -> assertEqual ("[" <> text <> "]") ast real
--     Left err -> trace err $ assertFailure "FUCK"

-- testTy :: String -> Ty -> Assertion
-- testTy text ast =
--   case Parser.parseTy text of
--     Right real -> assertEqual ("[" <> text <> "]") ast real
--     Left err -> trace err $ assertFailure "FUCK"

-- -- Tests

-- testLetDecls :: Assertion
-- testLetDecls =
--   do
--     let varDecl' x v = SItem NonRecursive (pvar x) (econst_int v)
--     let varDecl x = varDecl' x 4
--     let aDecl = varDecl "a"
--     let (@@) = EApp

--     identParsable "int"
--     tyParsable "int"
--     tyParsable "'0"
--     tyParsable "int -> int "
--     tyParsable "int -> (int->'1) -> '1"
--     tyParsable "unit"
--     tyParsable "unit -> int list"
--     tyParsable "string@local"
--     tyParsable "string@local -> bool"
--     tyParsable "'1 -> '1 @ local"
--     tyParsable "'1 @ local -> '1 list -> '1 list @ local"
--     tyParsable "'0 box @ local -> '0"

--     patternParsable "(x: int)"
--     patternParsable "x "
--     testExpr "a" (evar "a")
--     testExpr "-1 + 2 " (EApp (EApp (evar "+") (evar "-" `EApp` econst_int 1)) (econst_int 2))
--     testExpr "a+b" (exprBinOp (evar "+") (evar "a") (evar "b"))
--     testExpr "let x = 1 in x" (ELet NonRecursive (pvar "x") (econst_int 1) (EVar "x"))
--     testExpr
--       "let rec fix f = f (fix f) x in fix"
--       ( ELet
--           Recursive
--           (pvar "fix")
--           ( ELam
--               (pvar "f")
--               ((EVar "f" @@ (EVar "fix" @@ EVar "f")) @@ EVar "x")
--           )
--           (EVar "fix")
--       )

--     "let a = 4 " ==?=> Right (Program [aDecl])
--     testExpr "&a" (EBorrow (EVar "a"))
--     testExpr "f &a" (EVar "f" @@ EBorrow (EVar "a"))
--     "let a = let () = print_int 5 in 42"
--       ==?=> Right
--         ( Program
--             [ SItem
--                 NonRecursive
--                 (pvar "a")
--                 ( ELet
--                     NonRecursive
--                     PUnit
--                     ( EApp
--                         (EVar "print_int")
--                         (EConst (PConst_int 5))
--                     )
--                     (EConst (PConst_int 42))
--                 )
--             ]
--         )
--     programParsable
--       "let print_and_close (  file) =\n\
--       \let () = print_endline (read file) in\n\
--       \close file"
--     "let f (unique q) = q"
--       ==?=> Right (Program [SItem NonRecursive (PVar "f" PMNone) (ELam (PVar "q" PMUnique) (EVar "q"))])
--     programParsable
--       "let write_twice (unique x) =\n\
--       \write2 &x &x 54"
--     programParsable
--       "let f (n: int) = n"

-- -- "let a" ==?=> Nothing
-- -- "let = 4" ==?=> Nothing
-- -- "leta = 4" ==?=> Just (Program [StmtExpr (exprBinOp (CompOp EqOp) (ExprId "leta") (EConst (PrimValInt 4)))])

-- -- "let rec a = 4" ==?=> Nothing
-- -- "let rec a b = 4" ==?=> Just (Program [recFunDecl "a" ["b"]])
-- -- "let reca a b = 4" ==?=> Just (Program [funDecl "reca" ["a", "b"]])
-- -- "leta rec a b = 4" ==?=> Nothing
-- -- "letaa rec a b = 4" ==?=> Nothing
-- -- "let reca = 4" ==?=> Just (Program [varDecl "reca"])
-- -- "let recaa = 4" ==?=> Just (Program [varDecl "recaa"])
-- -- "let let a = 4" ==?=> Nothing
-- -- "let let = 4" ==?=> Nothing
-- -- "let leta = 4" ==?=> Just (Program [varDecl "leta"])

-- -- "let a = 4 let b = 8" ==?=> Just (Program [aDecl, bDecl])
-- -- "let a = 4\nlet b = 8" ==?=> Just (Program [aDecl, bDecl])
-- -- "let a = 4;;let b = 8" ==?=> Just (Program [aDecl, bDecl])
-- -- ";;let a = 4;;;;let b = 8;;;;;; ;;" ==?=> Just (Program [aDecl, bDecl])

-- -- testWhitespace :: TestTree
-- -- testWhitespace = testCase "whitespace" $ do
-- --   let decl =
-- --         Just
-- --           ( Program
-- --               [ StmtDecl
-- --                   ( DeclFun
-- --                       "f"
-- --                       False
-- --                       ( Fun
-- --                           (("a", Nothing) :| [])
-- --                           Nothing
-- --                           ( exprBinOp
-- --                               (ArithOp MulOp)
-- --                               (ExprId "a")
-- --                               ( EApp
-- --                                   (EApp (ExprId "a") (ExprId "f"))
-- --                                   (EConst (PrimValInt 4))
-- --                               )
-- --                           )
-- --                       )
-- --                   )
-- --               ]
-- --           )
-- --   let declAndApp =
-- --         Just
-- --           ( Program
-- --               [ StmtDecl
-- --                   ( DeclFun
-- --                       "f"
-- --                       False
-- --                       ( Fun
-- --                           (("a", Nothing) :| [])
-- --                           Nothing
-- --                           (exprBinOp (ArithOp MulOp) (ExprId "a") (ExprId "a"))
-- --                       )
-- --                   ),
-- --                 StmtExpr (EApp (ExprId "f") (EConst (PrimValInt 4)))
-- --               ]
-- --           )

-- --   "let f a = a * a f 4" ==?=> decl
-- --   "let f a = a * a\nf 4" ==?=> decl
-- --   "let f a = a * a;;f 4" ==?=> declAndApp

-- -- TODO : abs_max

-- -- testUnaryMinusOp :: TestTree
-- -- testUnaryMinusOp = testCase "unary minus operator" $ do
-- --   let prgStmtExpr e = Just (Program [StmtExpr e])

-- --   let zero = econst_int 0
-- --   let seven = econst_int 7
-- --   let a = EVar "a"
-- --   let b = EVar "b"

-- --   let minus = (EVar "-")

-- --   "-7" ==?=> prgStmtExpr (minus seven)
-- --   "- 7" ==?=> prgStmtExpr (minus seven)
-- --   "0 - 7"
-- --     ==?=> prgStmtExpr
-- --       (exprBinOp (EVar "-") zero seven)
-- --       "a - 7"
-- --     ==?=> prgStmtExpr (exprBinOp (EVar "-") a seven)
-- --   "a - b" ==?=> prgStmtExpr (exprBinOp (EVar "-") a b)
-- --   "a - -b" ==?=> prgStmtExpr (exprBinOp (EVar "-") a (minus b))

-- tests :: Test
-- tests =
--   TestList
--     [ TestCase testLetDecls
--     -- testWhitespace,
--     -- testUnaryMinusOp
--     ]
