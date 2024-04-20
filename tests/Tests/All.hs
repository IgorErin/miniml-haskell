module Tests.All(tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Common as TestLambda (runShow)

import qualified Parser as P (run)
import qualified Lexer  as L (Token, alexScanTokens)

import qualified Parsetree as T (Expr)
-- import qualified Inferencer as I (runInfer)

tests :: IO TestTree
tests = testGroup "Lambda" <$>
    sequence [
        lexerTests,
        parserTests
        -- inferTests
    ]

lexer :: String -> [L.Token]
lexer = L.alexScanTokens

lexerTests :: IO TestTree
lexerTests = TestLambda.runShow "Lexer" lexer

parser :: String -> T.Expr
parser = P.run . L.alexScanTokens

parserTests :: IO TestTree
parserTests = TestLambda.runShow "Parser" parser

-- infer :: String -> ET.Result
-- infer s = case parserProgram s of
--     Left message -> "Parsing filed with" ++ message
--     Right (Program program) -> I.runInfer program

--     in ET.run term

-- inferTests :: IO TestTree
-- inferTests = TestLambda.runShow "Typed" ((ET.ShowTyped <$>) . infer)
