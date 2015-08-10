module Main where

import Prelude
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token
import Text.Parsing.Parser.Pos

import Control.Alt
import Control.Apply ((<*))
import Data.Functor (($>))
import Control.Alternative

import Data.Array(some)
import Data.String(fromCharArray)
import Data.List(List(..),fromList,zip)

import qualified Data.Map as M

type P a = Parser String a

quoted :: forall a. P a -> P a
quoted p = between (string "\"") (string "\"") p

chars :: P String
chars = fromCharArray <$> some char
  where
  char = satisfy (\c -> c /= '"' && c /= '\n' && c /= ',')

qchars :: P String
qchars = fromCharArray <$> some (qchar <|> escapedQuote)
  where
  escapedQuote :: P Char
  escapedQuote = string "\"\"" $> '"'
  qchar = satisfy (\c -> c /= '"')

field :: P String
field = quoted qchars <|> chars

row :: P (List String)
row = field `sepBy1` string ","

file :: P (List (List String))
file = row `sepEndBy` string "\n" <* eof

fileHeaded :: P (List (M.Map String String))
fileHeaded = do
  f <- file
  return $ case f of
    Nil -> Nil
    Cons header rows -> mkRow header <$> rows
  where
    mkRow header row = M.fromList $ zip header row
