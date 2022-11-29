module Parsing.CSV where

import Prelude hiding (between)
import Data.Map as M
import Control.Alt ((<|>))
import Data.Array (some)
import Data.Foldable (all)
import Data.List (List(..), zip)
import Data.String.CodeUnits (fromCharArray, toCharArray, singleton)
import Parsing (Parser)
import Parsing.Combinators (sepEndBy, sepBy, between)
import Parsing.String (eof, satisfy, string)

type P a = Parser String a

type Parsers a =
  {
    quoted :: (P a -> P a),
    chars :: P String,
    qchars :: P String,
    field :: P String,
    row :: P (List String),
    file :: P (List (List String)),
    fileHeaded :: P (List (M.Map String String))
  }

makeQuoted :: forall a. String -> (P a -> P a)
makeQuoted q = between (string q) (string q)

makeChars :: String -> P String
makeChars xs = do
  fromCharArray <$> some char
  where
    char = satisfy $ excluded xs
    excluded ys = \x -> all identity $ terms ys <*> [x]
    terms ys = map (/=) $ toCharArray ys

makeQchars :: Char -> P String
makeQchars c = fromCharArray <$> some (qchar <|> escapedQuote)
  where
  escapedQuote :: P Char
  escapedQuote = (string $ (singleton c <> singleton c)) $> c
  qchar = satisfy (\c' -> c' /= c)

makeField :: (P String -> P String) -> P String -> P String -> P String
makeField qoutes qoutedChars purechars = qoutes qoutedChars <|> purechars <|> string ""

makeRow :: String -> P String -> P (List String)
makeRow s f = f `sepBy` string s

makeFile :: String -> P (List String) -> P (List (List String))
makeFile s r = r `sepEndBy` string s <* eof

makeFileHeaded :: P (List (List String)) -> P (List (M.Map String String))
makeFileHeaded file = do
  f <- file
  pure $ case f of
    Nil -> Nil
    Cons header rows -> mkRow header <$> rows
  where
    mkRow header row' = M.fromFoldable $ zip header row'

makeParsers :: Char -> String -> String -> Parsers String
makeParsers quote seperator eol = do
  let quoted' = makeQuoted $ singleton quote
  let chars' = makeChars $ (singleton quote) <> seperator <> eol
  let qchars' = makeQchars quote
  let field' = makeField quoted' qchars' chars'
  let row' = makeRow seperator field'
  let file' = makeFile eol row'
  let fileHeaded' = makeFileHeaded file'
  {
    quoted: quoted',
    chars: chars',
    qchars: qchars',
    field: field',
    row: row',
    file: file',
    fileHeaded: fileHeaded'
  }

defaultParsers :: Parsers String
defaultParsers = makeParsers '"' "," "\n"
