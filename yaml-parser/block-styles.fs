module YamlParser.BlockStyle

open YamlParser.Types
open YamlParser.Primitives

open FParsec

let sequence indent =
  block (
    pstring "-" 
    >>? whitespaces1
    >>. parser
    .>> lineBreak
    .>> whitespaces
    <!> "seq-item"
  ) <!> "seq"
  |>> Sequence

let sequence' context indent =
  let pitem indent =
        checkIndent indent
    >>? pstring "-"
    >>? whitespaces1
    >>. parser context indent
    >>. lineBreak
    <!> "seq-item"
  
  indentation >>= fun indent ->
    many1 (checkIndent indent >>. pitem) <!> "seq"