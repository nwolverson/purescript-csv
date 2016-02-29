# purescript-csv
Simple configurable PureScript CSV parser.

Based on `purescript-parsing`. Parse list of rows (getting a simple list of fields) or using header row (getting
a map from field name to value).

## Example (copy into your REPL)

    import Prelude

    import Text.Parsing.CSV (defaultParsers, makeParsers)
    import Text.Parsing.Parser (runParser)

    import Data.List
    import Data.Either

    let isTrue exp = either (\_-> false) (== (exp :: List (List String)))

    let testFile = "a,,c,\n,1,2,3\n\"x\",\"field,quoted\",z\n" :: String
    let testResult = toList $ toList <$> [["a", "", "c", ""], ["", "1", "2", "3"], ["x", "field,quoted", "z"], [""]]

    let parseResult = runParser testFile defaultParsers.file
    parseResult
    isTrue testResult parseResult

    :t runParser testFile defaultParsers.file`

If you want to create your own parsers using different field seperators, differnt quotation chars and different line endings (instead of using the defaultParsers) you can do that by

    excelParsers = makeParsers '\'' ";" "\r\n"

This will generate a value of type    

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

In this specific case using `'` as a quotation character a `;` as a field seperator and using windows `\r\n` line seperators instead of the *nix `\n`.

The easiest and fastest way of course is just to use the `defaultParsers` who are defined as `defaultParsers = makeParsers '"' "," "\n"`. 
