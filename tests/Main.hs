module Main(main) where

import Test.Tasty

import qualified Tests.All as All (tests)

tests :: IO TestTree
tests = testGroup "Main" <$> sequence [ All.tests ] -- TODO

main :: IO ()
main = defaultMain =<< tests