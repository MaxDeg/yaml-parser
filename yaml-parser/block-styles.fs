module YamlParser.BlockStyle

open YamlParser.Types
open YamlParser.Primitives

open FParsec

let private blockParser, blockParserRef = createParserForwardedToRef<Value, State>()

module Scalars =
  let parser : Parser<Value, State> = fail "not implemented"

module Collections =
  let private psep = 
        whitespaces
    >>? skipMany1LineBreak
    >>? skipWhitespaces
    >>? checkIndentation


  let indentedBlock =
    blockParser //<|> comments

  let sequence =
    let pitem =
          pstring "-"
      >>? followedBy whitespaces1
      >>. withContext BlockIn indentedBlock
      <!> "seq-item"

    withIndentation >>? many1 (checkIndentation >>? pitem)
    <!> "seq"
    |>> Sequence


  let mapping =
    let explicitItem = 
          pstring "?"
      >>.  whitespaces1
      >>.  withContext BlockOut (withHigherIndentation parser)
      .>>  manyLineBreak
      .>>  checkIndentation
      .>>  pstring ":"
      .>>  whitespaces1
      .>>. (withContext BlockOut parser)
      <!>  "explicit-map-item"
    
    let implicitItem =
          withContext BlockOut (withHigherIndentation parser)
      .>>?  whitespaces
      .>>? pstring ":"
      .>>  whitespaces1
      .>>. (withContext BlockKey parser)
      <!>  "implicit-map-item"

    let pitem =
      explicitItem// Buggy code <|> implicitItem
    
    withSameOrHigherIndentation <|
      sepBy1 pitem psep <!> "map"
      |>> (Map.ofList >> Mapping)

  let parser = sequence //<|> mapping


let flowInBlock = withContext FlowOut pseparate >>. FlowStyle.parser .>> comments <!> "flow in block"

blockParserRef :=  Collections.parser
             //  <|> Scalars.parser
               <|> flowInBlock

let parser = blockParser
