module YamlParser.Parser

open YamlParser
open YamlParser.Types
open YamlParser.Primitives

open FParsec

parserRef := fun ctx indent ->
  choice  [ BlockStyle.parser ctx indent
            FlowStyle.parser ctx indent
            empty
          ]

let bareDocument =
      whitespaces
  >>. parser BlockIn 0L
  .>> whitespaces
  .>> eof

let run str = 
  run bareDocument str
