module Tests.All(tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Common as TestLambda (runShow)

import qualified Parser as P (Parser, Result, parseProgram)

tests :: IO TestTree
tests = testGroup "Lambda" <$>
    sequence [
        -- lexerTests,
        parserTests
        -- inferTests
    ]

-- lexer :: String -> [LL.Token]
-- lexer = LL.alexScanTokens

-- lexerTests :: IO TestTree
-- lexerTests = TestLambda.runShow "Lexer" lexer

parserProgram :: String -> P.Result
parserProgram = P.parseProgram

parserTests :: IO TestTree
parserTests = TestLambda.runShow "Parser" parserProgram

-- infer :: String -> ET.Result
-- infer s =
--     let term = parser s
--     in ET.run term

-- inferTests :: IO TestTree
-- inferTests = TestLambda.runShow "Typed" ((ET.ShowTyped <$>) . infer)
