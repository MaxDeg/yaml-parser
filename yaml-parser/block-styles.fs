module YamlParser.BlockStyle

open YamlParser.Types
open YamlParser.Primitives

open FParsec

let private blockParser, blockParserRef =
  createParserForwardedToRef<Value, State>()

module Scalars =
  let parser : Parser<Value, State> = fail "not implemented"

module Collections =
  let private indentedBlockParser, indentedBlockParserRef =
    createParserForwardedToRef<Value, State>()
  
  let private psep = 
        whitespaces
    >>? skipMany1LineBreak
    >>? skipWhitespaces
    >>? checkIndentation

  let sequence =
    let seqEntry =
      indent'
      >>? pstring "-"
      >>? followedBy whitespaces1
      >>. withContext BlockIn indentedBlockParser
      <!> "seq-entry"

    indentMore >>? many1 seqEntry
    <!> "seq"
    |>> Sequence

  let mapping =
    let explicitEntry =
      let explicitKey =
        skipChar '?' >>. withContext BlockOut indentedBlockParser
        <!> "map-explicit-key"
      let explicitValue =
        indent'
        >>? skipChar ':'
        >>. withContext BlockOut indentedBlockParser
        <!> "map-explicit-value"

      explicitKey .>>.? (explicitValue <|>% Empty)

    let implicitEntry =
      let implicitKey = 
        withContext BlockKey
          ((FlowStyle.Collections.jsonContent <|> FlowStyle.Scalars.yamlParser)
          .>>? opt pseparate)
          <!> "map-implicit-key"

      let implicitValue =
        skipChar ':' 
          >>. (withContext BlockOut blockParser
              <|> (preturn Empty .>> comments))
          <!> "map-implicit-value"

      (implicitKey <|>% Empty) .>>.? implicitValue

    let mapEntry = explicitEntry <|> implicitEntry

    indentMore >>? many1 mapEntry
    <!> "mapping"
    |>> (Map.ofList >> Mapping)

  indentedBlockParserRef := 
    choice [ // compact sequence
             (indentMore1 >>? sequence)
             // compact mapping
             (indentMore1 >>? mapping)
             blockParser
             preturn Empty .>> comments
           ]
           <!> "indented-block"

  let parser =
    let seqSpaces =
      getUserState >>= fun state ->
      match state.context with
      | BlockOut -> indent -1L sequence
      | _ -> sequence

    comments >>. (seqSpaces <|> mapping) <!> "block-collection"


let flowInBlock =
  indent1 (
    withContext FlowOut (pseparate >>? FlowStyle.parser))
  .>> comments
  <!> "flow in block"


blockParserRef :=  //  <|> Scalars.parser
  Collections.parser
  <|> flowInBlock
  <!> "block-parser"

let parser = blockParser
