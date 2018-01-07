module YamlParser.BlockStyle

open YamlParser.Types
open YamlParser.Primitives

open FParsec

module Scalars =
  let parser ctx indent = fail "not implemented"

module Collections =
  
  let sequence _ minimalIndent =
    let pitem indent =
          whitespaces
      >>? checkIndentation indent
      >>? pstring "-"
      >>? whitespaces1
      >>. parser BlockIn indent
      .>> (skipLineBreak1 <|> eof)
      <!> "seq-item"
  
    indentation minimalIndent >>= fun indent ->
      many1 (pitem indent) <!> "seq"
      |>> Sequence


  let mapping _ minimalIndent =
    let explicitItem indent = 
          pstring "?"
      >>. whitespaces1
      >>.  parser BlockOut (indent + 1L)
      .>>  lineBreak
      .>>  checkIndentation indent
      .>>  pstring ":"
      .>> whitespaces1
      .>>. parser BlockOut indent
      <!>  "explicit-map-item"
    
    let implicitItem indent =
      fail ""

    let pitem indent =
          whitespaces
      >>? checkIndentation indent
      >>. (explicitItem indent <|> implicitItem indent)
      .>> (skipLineBreak1 <|> eof)

    indentation minimalIndent >>= fun indent ->
      many1 (pitem indent) <!> "map"
      |>> (Map.ofList >> Mapping)

  let parser ctx indent = 
    choice [ sequence ctx (match ctx with BlockOut -> indent - 1L | _ -> indent)
             mapping ctx indent
           ]


let parser ctx indent = Collections.parser ctx indent
                    //<|> Scalars.parser ctx indent
