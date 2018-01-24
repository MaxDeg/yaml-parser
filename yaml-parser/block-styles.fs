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
  
  module Sequence =
    let seqEntry =
      pstring "-"
      >>? followedBy whitespaces1
      >>. withContext BlockIn indentedBlockParser
      <!> "seq-entry"
    
    let seq =
      indentMore >>? many1 (indent >>? seqEntry)
      <!> "seq"
      |>> Sequence

    let compactSeq =
      seqEntry .>>. many (indent >>? seqEntry)
      <!> "compact-seq"
      |>> fun (h, t) -> Sequence(h::t)

  module Mapping =
    let explicitEntry =
      let explicitKey =
        skipChar '?' >>. withContext BlockOut indentedBlockParser
        //<!> "map-explicit-key"
      let explicitValue =
        indent
        >>? skipChar ':'
        >>. withContext BlockOut indentedBlockParser
        //<!> "map-explicit-value"

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

    let mapEntry =
      explicitEntry <|> implicitEntry
      <!> "map-entry"

    let map =
      indentMore >>? many1 (indent >>? mapEntry)
      <!> "mapping"
      |>> (Map.ofList >> Mapping)

    let compactMap =
      mapEntry .>>. many (indent >>? mapEntry)
      <!> "mapping"
      |>> fun (h, t) -> h::t |> Map.ofList |> Mapping

  indentedBlockParserRef := 
    choice [ // compact sequence
             (indentMore >>? Sequence.compactSeq)
             // compact mapping
             (indentMore >>? Mapping.compactMap)
             blockParser
             preturn Empty .>> comments
           ]
           <!> "indented-block"

  let parser =
    let seqSpaces =
      getUserState >>= fun state ->
      match state.context with
      | BlockOut -> 
          indentMinus1 >>? Sequence.seq
      | _ ->
          Sequence.seq

    comments >>. (seqSpaces <|> Mapping.map) <!> "block-collection"


let flowInBlock =
  indentPlus1
  >>? withContext FlowOut (pseparate >>? FlowStyle.parser)
  .>> comments
  <!> "flow in block"


blockParserRef :=  //  <|> Scalars.parser
  Collections.parser
  <|> flowInBlock
  <!> "block-parser"

let parser = blockParser
