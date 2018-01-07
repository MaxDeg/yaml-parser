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
      >>? pstring "-"
      >>? whitespaces1
      >>. parser BlockIn indent
      <!> "seq-item"

    let psep indent = 
          whitespaces
      >>? skipLineBreak1
      >>? checkIndentation indent

    indentation minimalIndent >>= fun indent ->
      //many1 (pitem indent) <!> "seq"
      sepBy1 (pitem indent) (psep indent) <!> "seq"
      |>> Sequence


  let mapping _ minimalIndent =
    let explicitItem indent = 
          pstring "?"
      >>.  whitespaces1
      >>.  parser BlockOut (indent + 1L)
      .>>  lineBreak
      .>>  checkIndentation indent
      .>>  pstring ":"
      .>>  whitespaces1
      .>>. parser BlockOut indent
      <!>  "explicit-map-item"
    
    let implicitItem indent =
          parser BlockOut (indent + 1L)
      .>>?  whitespaces
      .>>? pstring ":"
      .>>  whitespaces1
      .>>. parser BlockKey indent
      <!>  "implicit-map-item"

    let pitem indent =
          whitespaces
      >>? checkIndentation indent
      >>. (explicitItem indent <|> implicitItem indent)
    
    let psep indent = 
          whitespaces
      >>? skipLineBreak1
      >>? checkIndentation indent

    indentation minimalIndent >>= fun indent ->
      //many1 (pitem indent) <!> "map"
      sepBy1 (pitem indent) (psep indent) <!> "map"
      |>> (Map.ofList >> Mapping)

  let parser ctx indent = 
    choice [ sequence ctx (match ctx with BlockOut -> indent - 1L | _ -> indent)
             mapping ctx indent
           ]


let parser ctx indent = Collections.parser ctx indent
                    //<|> Scalars.parser ctx indent
