module Text.Parsing.CSV where

import Prelude ((<$>), return, ($), bind, (/=), map, id, (<*>), (++))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (sepEndBy, sepBy1, between)
import Text.Parsing.Parser.String (eof, string, satisfy)

import Control.Alt ((<|>))
import Control.Apply ((<*))

import Data.Functor (($>))
import Data.Array (some)
import Data.String (fromCharArray, toCharArray)
import Data.Char (toString)
import Data.List (List(..),zip)
import Data.Foldable (all)
import Data.Map as M

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
    excluded xs = \x -> all id $ terms xs <*> [x]
    terms xs = map (/=) $ toCharArray xs

makeQchars :: Char -> P String
makeQchars c = fromCharArray <$> some (qchar <|> escapedQuote)
  where
  escapedQuote :: P Char
  escapedQuote = (string $ (toString c ++ toString c)) $> c
  qchar = satisfy (\c' -> c' /= c)

makeField :: (P String -> P String) -> P String -> P String -> P String
makeField qoutes qoutedChars purechars = qoutes qoutedChars <|> purechars <|> string ""

makeRow :: String -> P String -> P (List String)
makeRow s f = f `sepBy1` string s

makeFile :: String -> P (List String) -> P (List (List String))
makeFile s r = r `sepEndBy` string s <* eof

makeFileHeaded :: P (List (List String)) -> P (List (M.Map String String))
makeFileHeaded file = do
  f <- file
  return $ case f of
    Nil -> Nil
    Cons header rows -> mkRow header <$> rows
  where
    mkRow header row' = M.fromList $ zip header row'

makeParsers :: Char -> String -> String -> Parsers String
makeParsers quote seperator eol = do
  let quoted' = makeQuoted $ toString quote
  let chars' = makeChars $ (toString quote) ++ seperator ++ eol
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
