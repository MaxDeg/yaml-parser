module YamlParser.BlockStyle

open YamlParser.Types
open YamlParser.Primitives

open FParsec

module Scalars =
  let parser ctx indent = fail "not implemented"

module Collections =
  let private psep indent = 
        whitespaces
    >>? skipMany1LineBreak
    >>? skipWhitespaces
    >>? checkIndentation indent
  
  let sequence _ minimalIndent =
    let pitem indent =
          pstring "-"
      >>? whitespaces1
      >>. parser BlockIn indent
      <!> "seq-item"

    indentation minimalIndent >>= fun indent ->
      sepBy1 (pitem indent) (psep indent) <!> "seq"
      |>> Sequence


  let mapping _ minimalIndent =
    let explicitItem indent = 
          pstring "?"
      >>.  whitespaces1
      >>.  parser BlockOut (indent + 1L)
      .>>  manyLineBreak
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
      (explicitItem indent)// Buggy code <|> implicitItem indent)
    
    indentation minimalIndent >>= fun indent ->
      sepBy1 (pitem indent) (psep indent) <!> "map"
      |>> (Map.ofList >> Mapping)

  let parser ctx indent = 
        sequence ctx (match ctx with BlockOut -> indent - 1L | _ -> indent)
    <|> mapping ctx indent


let parser ctx indent = Collections.parser ctx indent
                    //<|> Scalars.parser ctx indent
