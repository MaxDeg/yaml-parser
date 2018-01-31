module YamlParser.Parser

open YamlParser
open YamlParser.Types
open YamlParser.Primitives

open FParsec

parserRef := choice  [ BlockStyle.parser
                       FlowStyle.parser
                       //empty
                     ]

let bareDocument = BlockStyle.parser .>> eof

let run str = 
  runParserOnString
    bareDocument
    { indent      = 0L
      indentType  = AutoDetect
      context     = BlockIn
      chomping    = None }
    "" // stream name
    str
  |> function
  | Success(result, _, _) -> Result.Ok result
  | Failure(error, _, _) -> Result.Error error
