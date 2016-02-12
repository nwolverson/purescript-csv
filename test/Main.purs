module Test.Main where

import Prelude (class Eq, Unit, (==), (&&), return, ($), bind, (++), (<$>))

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Test.Unit (TIMER, test, runTest)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT ())
import Text.Parsing.CSV (P, fileHeaded, file, row, field, chars)
import Data.Either (Either(Left, Right))
import Data.Maybe (fromMaybe)

import Text.Parsing.Parser (runParser)

import Data.List(toList,List(),head)
import Data.Map as M

parseTrue :: forall a. P a -> (a -> Boolean) -> String -> Boolean
parseTrue parser expectation input =
  case runParser input parser of
    Right result -> expectation result
    Left _ -> false

parses :: forall a. (Eq a) => P a -> a -> String -> Boolean
parses parser expected input = parseTrue parser (\r -> expected == r) input

noparse :: forall a. (Eq a) => P a -> String -> Boolean
noparse parser input =
  case runParser input parser of
    Right _ -> false
    Left _ -> true

testFile :: String
testFile = """a,b,c
1,2,3
"x","y",z"""

testFileResult :: List (List String)
testFileResult = toList $ toList <$> [
  ["a", "b", "c"],
  ["1", "2", "3"],
  ["x", "y", "z"]
]

main :: forall a. Eff (testOutput :: TESTOUTPUT, avar :: AVAR, timer :: TIMER | a) Unit
main = runTest do
  test "chars" do
    assert "parses chars" $ parses chars "abc" "abc"
    assert "doesn't parse with quote" $ parses chars "ab" "ab\"c"
  test "unquoted field" do
    assert "" $ parses field "abc123" "abc123"
  test "quoted field" do
    assert "" $ parses field "abc123" "\"abc123\""
  test "quoted field with quotes" do
    assert "doesn't allow quote escape" $ parses field "x\"y" "\"x\"\"y\""
  test "newlines" do
    assert "can't have newline in unquoted field" $ parses field "a" "a\nb"
    assert "can have newline in quoted field" $ parses field "a\nb" "\"a\nb\""
  test "row" do
    assert "failed basic row" $ parses row (toList $ ["a", "b", "c"]) "a,b,c"
    assert "failed quoted row" $ parses row (toList $ ["a", "b", "c"]) "\"a\",\"b\",\"c\""
  test "file" do
    assert "single line file" $ parses file testFileResult "a,b,c\n1,2,3\nx,y,z"
    assert "didn't parse file" $ parses file testFileResult testFile
    assert "didn't parse file with trailing newline" $ parses file testFileResult $ testFile ++ "\n"
  test "fileHeaded" do
    assert "headed lookup" $ parseTrue fileHeaded (\res -> fromMaybe false $ do
      row <- head res
      a <- M.lookup "a" (row :: M.Map String String)
      b <- M.lookup "b" row
      return $ (a :: String) == "1" && b == "2") testFile
