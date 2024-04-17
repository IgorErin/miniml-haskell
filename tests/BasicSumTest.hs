module Main where

import Data.Set (fromList)
import InferTest (inferTests)
import Inferencer
import qualified ParserTest (tests)
import qualified System.Exit as Exit
import Test.HUnit
import Typedtree

test1 :: Test
test1 = TestCase (assertEqual "should return 3" 3 (basicSum 1 2))

test2occurs :: Test
test2occurs = TestCase (assertEqual "" True (occurs_in 1 (TyVar 1)))

test3freeVars :: Test
test3freeVars = TestCase (assertEqual "" (fromList [1, 2]) (free_vars (Arrow (TyVar 1) $ TyVar 2)))

tests :: Test
tests =
  TestList
    [ TestLabel "test1" test1,
      TestLabel "Typs" test2occurs,
      TestLabel "FreeVars" test3freeVars,
      TestLabel "InferTests" inferTests,
      TestLabel "" ParserTest.tests
    ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
