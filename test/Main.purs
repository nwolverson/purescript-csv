module Test.Main where

import Prelude (class Eq, Unit, (==), (&&), return, ($), bind, (++), (<$>))

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)


import Test.Unit (TIMER, test, runTest)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT ())

import Text.Parsing.CSV (P, defaultParsers, makeParsers, Parsers)
import Text.Parsing.Parser (runParser)

import Data.Either (Either(Left, Right))
import Data.Maybe (fromMaybe)
import Data.List(toList,List(),head)
import Data.Map as M

excelParsers :: Parsers String
excelParsers = makeParsers '\'' ";" "\r\n"

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

testData :: Array (Array String)
testData = [
  ["a", "b", "c"],
  ["1", "2", "3"],
  ["x", "y", "z"]
]

testFileResult :: List (List String)
testFileResult = toList $ toList <$> testData

testFileEmptyEndLineResult :: List (List String)
testFileEmptyEndLineResult = toList $ toList <$> testData ++ [[""]]

main :: forall a. Eff (testOutput :: TESTOUTPUT, avar :: AVAR, timer :: TIMER | a) Unit
main = runTest do
  test "chars" do
    assert "parses chars" $ parses defaultParsers.chars "abc" "abc"
    assert "doesn't parse with quote" $ parses defaultParsers.chars "ab" "ab\"c"
  test "unquoted field" do
    assert "" $ parses defaultParsers.field "abc123" "abc123"
  test "quoted field" do
    assert "quoted with \": " $ parses defaultParsers.field "abc123" "\"abc123\""
    assert "cant parse ' quotation': " $ parses excelParsers.field "abc123" "'abc123'"
    assert "can have seperator in quoted field" $ parses excelParsers.field "abc;123" "'abc;123'"
  test "quoted field with quotes" do
    assert "doesn't allow quote escape" $ parses defaultParsers.field "x\"y" "\"x\"\"y\""
  test "newlines" do
    assert "can't have newline in unquoted field" $ parses defaultParsers.field "a" "a\nb"
    assert "can have newline in quoted field" $ parses defaultParsers.field "a\nb" "\"a\nb\""
  test "row" do
    assert "failed basic row" $ parses defaultParsers.row (toList $ ["a", "b", "c"]) "a,b,c"
    assert "failed quoted row" $ parses defaultParsers.row (toList $ ["a", "b", "c"]) "\"a\",\"b\",\"c\""
    assert "failed basic row" $ parses excelParsers.row (toList $ ["a", "b", "c"]) "a;b;c"
    assert "failed quoted row" $ parses excelParsers.row (toList $ ["a", "b", "c"]) "'a';'b';'c'"
  test "row with empty fields" do
    assert "failed empty fields" $ parses defaultParsers.row (toList $ ["a", "", "c"]) "a,,c"
    assert "failed empty fields at begining" $ parses defaultParsers.row (toList $ ["", "a", "b", "c"]) ",a,b,c"
    assert "failed empty fields at end" $ parses defaultParsers.row (toList $ ["a", "b", "c", ""]) "a,b,c,"
  test "file" do
    assert "single line file" $ parses defaultParsers.file testFileResult "a,b,c\n1,2,3\nx,y,z"
    assert "single line file with windows eols" $ parses excelParsers.file testFileResult "a;b;c\r\n1;2;3\r\nx;y;z"
    assert "didn't parse file" $ parses defaultParsers.file testFileResult testFile
    assert "didn't parse file with trailing newline" $ parses defaultParsers.file testFileEmptyEndLineResult $ testFile ++ "\n"
  test "fileHeaded" do
    assert "headed lookup" $ parseTrue defaultParsers.fileHeaded (\res -> fromMaybe false $ do
      row <- head res
      a <- M.lookup "a" (row :: M.Map String String)
      b <- M.lookup "b" row
      return $ (a :: String) == "1" && b == "2") testFile
