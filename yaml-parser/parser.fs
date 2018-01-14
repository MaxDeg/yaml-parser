module YamlParser.Parser

open Prelude

open YamlParser
open YamlParser.Types
open YamlParser.Primitives

open FParsec

parserRef := choice  [ BlockStyle.parser
                       FlowStyle.parser
                       //empty
                     ]

let bareDocument = BlockStyle.parser

let run str = 
  runParserOnString
    bareDocument
    { indent  = 0L
      context = BlockIn }
    "" // stream name
    str
