module Test.Main where

import Prelude (class Eq, Unit, (==), (&&), return, ($), bind, (++), (<$>))

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Test.Unit (TIMER, test, runTest)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT ())
import Text.Parsing.CSV (P, defaultParsers, makeParsers)
import Data.Either (Either(Left, Right))
import Data.Maybe (fromMaybe)

import Text.Parsing.Parser (runParser)

import Data.List(toList,List(),head)
import Data.Map as M

excelParser = makeParsers '\'' ";" "\r\n"

parseTrue :: forall a. P a -> (a -> Boolean) -> String -> Boolean
parseTrue parser expectation input =
  case runParser input parser of
    Right result -> expectation result
    Left _ -> false

parses :: forall a. (Eq a) => P a -> a -> String -> Boolean
parses parser expected input = parseTrue parser (\r -> expected == r) input

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
    assert "parses chars" $ parses defaultParsers.chars "abc" "abc"
    assert "doesn't parse with quote" $ parses defaultParsers.chars "ab" "ab\"c"
  test "unquoted field" do
    assert "" $ parses defaultParsers.field "abc123" "abc123"
  test "quoted field" do
    assert "quoted with \": " $ parses defaultParsers.field "abc123" "\"abc123\""
    assert "cant parse ' quotation': " $ parses excelParser.field "abc123" "'abc123'"
    assert "can have seperator in quoted field" $ parses excelParser.field "abc;123" "'abc;123'"
  test "quoted field with quotes" do
    assert "doesn't allow quote escape" $ parses defaultParsers.field "x\"y" "\"x\"\"y\""
  test "newlines" do
    assert "can't have newline in unquoted field" $ parses defaultParsers.field "a" "a\nb"
    assert "can have newline in quoted field" $ parses defaultParsers.field "a\nb" "\"a\nb\""
  test "row" do
    assert "failed basic row" $ parses defaultParsers.row (toList $ ["a", "b", "c"]) "a,b,c"
    assert "failed quoted row" $ parses defaultParsers.row (toList $ ["a", "b", "c"]) "\"a\",\"b\",\"c\""
    assert "failed basic row" $ parses excelParser.row (toList $ ["a", "b", "c"]) "a;b;c"
    assert "failed quoted row" $ parses excelParser.row (toList $ ["a", "b", "c"]) "'a';'b';'c'"
  test "file" do
    assert "single line file" $ parses defaultParsers.file testFileResult "a,b,c\n1,2,3\nx,y,z"
    assert "single line file with windows eols" $ parses excelParser.file testFileResult "a;b;c\r\n1;2;3\r\nx;y;z"
    assert "didn't parse file" $ parses defaultParsers.file testFileResult testFile
    assert "didn't parse file with trailing newline" $ parses defaultParsers.file testFileResult $ testFile ++ "\n"
  test "fileHeaded" do
    assert "headed lookup" $ parseTrue defaultParsers.fileHeaded (\res -> fromMaybe false $ do
      row <- head res
      a <- M.lookup "a" (row :: M.Map String String)
      b <- M.lookup "b" row
      return $ (a :: String) == "1" && b == "2") testFile
