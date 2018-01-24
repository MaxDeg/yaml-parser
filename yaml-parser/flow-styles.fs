module YamlParser.FlowStyle

open Prelude

open YamlParser.Types
open YamlParser.Primitives

open FParsec

let private flowParser, flowParserRef = createParserForwardedToRef<Value, State>()

module Scalars =
  let plainSafeFilter context exceptChars c =
      match context with 
      | FlowOut | BlockKey
      | BlockIn | BlockOut  -> 
          Array.append exceptChars spaceChars
          |> Array.contains c
          |> not

      | FlowIn | FlowKey    -> 
          [| spaceChars; flowIndicators; exceptChars |]
          |> Array.concat
          |> Array.contains c
          |> not
          
  let plainSafe = 
    getUserState >>= fun { context = context } ->
      many1Satisfy (plainSafeFilter context [||])

  let plain =
    let firstChar =
      choice [ Array.append spaceChars indicators |> skipNoneOf
               skipAnyOf ":?-" .>>? followedBy plainSafe
             ]
             //<!> "plain-first-char"

    let plainChar = 
      getUserState >>= fun { context = context } ->
        choice [ many1Satisfy (plainSafeFilter context [| '#'; ':' |])
                 pchar ':' .>>.? plainSafe |>> (fun (a, b) -> sprintf "%c%s" a b)
                 previousCharSatisfies (fun c -> not <| Array.contains c spaceChars) >>? pstring "#"
               ]
        <!> "plain-char"
    
    let plainLine =
      manyStrings (attempt <| pipe2 whitespaces plainChar (+))
      <!> "plain-line"

    let plainOneLine =
      followedBy firstChar >>. plainLine
      <!> "plain-one-line"

    let plainNextLines =
      manyStrings
        (attempt <| pipe3 folded plainChar plainLine (fun a b c -> a + b + c))
      <!> "plain-next-lines"
    
    getUserState >>= fun { context = ctx } ->
      match ctx with
      | FlowOut | FlowIn
      | BlockOut | BlockIn ->
          pipe2 plainOneLine plainNextLines (+)
          |>> String

      | BlockKey | FlowKey ->
          plainOneLine |>> String    

  let doubleQuoted = 
    between
      (skipChar '"')
      (skipChar '"')
      (manySatisfy (fun c -> c <> '"'))
    <!> "double-quoted"
    |>> String
    
  let singleQuoted = 
    let escapedChar = pstring "''" >>% "'"
    let acceptedChar = 
      manySatisfy (fun c -> c <> '\'' && c > '\x20' && c <= '\u10FF')

    let folded =   (opt separateInLine)
               >>? many1 (whitespaces >>? lineBreak <!> "folded-line")
               .>> whitespaces
               <!> "folded"
               |>> fun s -> if s.Length > 1 then
                              List.skip 1 s
                              |> List.map string
                              |> String.concat ""
                            else " "

    let singleline =
      stringsSepBy1
        (manyStrings
          (whitespaces
           .>>.? (many1Satisfy (fun c -> c <> '\'' && c > '\x20' && c <= '\u10FF'))
           |>> fun (a, b) -> a + b))
        escapedChar
      <!> "singleline"

    
    getUserState >>= fun { context = ctx } ->
      let text = 
        match ctx with
        | FlowIn   | FlowOut
        | BlockIn  | BlockOut -> 
          pipe2
            (stringsSepBy1 singleline folded)
            whitespaces
            (+)

        | BlockKey | FlowKey -> 
          stringsSepBy 
            (manySatisfy (fun c -> c <> '\'' && (c = '\x09' || (c >= '\x20' && c <= '\u10FF'))))
            escapedChar

      let skipSingleQuote = skipChar '\''

      between skipSingleQuote skipSingleQuote text
      <!> "single-quoted"
      |>> String

  let yamlParser = 
    choice [ pnull
             ptrue
             pfalse
             pnumber
             plain
           ]
           <!> "yaml-parser"

  let jsonParser = singleQuoted <|> doubleQuoted <!> "json-parser"
  
  let parser = jsonParser <|> yamlParser <!> "scalar"


module Collections = 
  let private flowPairParser, flowPairParserRef = 
    createParserForwardedToRef<Value, State>()
  
  let psep =
    skipChar ',' .>> opt pseparate

  let sequence =
    let pitem = 
      (flowPairParser <|> flowParser) .>> opt pseparate
      <!> "flow-seq-entry"
    
    let seq =
      getUserState >>= fun { context = context } ->
      let inFlow = 
        match context with
        | FlowOut -> FlowIn
        | FlowIn -> FlowIn
        | BlockKey -> FlowKey
        | FlowKey -> FlowKey
        | c -> c

      withContext inFlow <| sepEndBy pitem psep

    between (skipChar '[') (skipChar ']') (opt pseparate >>. seq)
    <!> "flow-sequence"
    |>> Sequence

  let private pmapping, pmappingRef = createParserForwardedToRef<Value, State>()

  module internal Mapping =
    let separateValue =
      skipChar ':'
        >>? notFollowedBy Scalars.plainSafe
        >>. ((pseparate >>? flowParser) <|>% Empty)
        //<!> "separate-value"
    
    let adjacentValue =
      skipChar ':'
        >>. (opt pseparate >>? flowParser <|>% Empty)
        //<!> "adjacent-value"

    let implicitEntry =
      choice [ (Scalars.jsonParser <|> sequence <|> pmapping <|>% Empty) .>> opt pseparate .>>.? adjacentValue
               Scalars.yamlParser .>>.? (opt pseparate >>? separateValue <|>% Empty)
               preturn Empty .>>.? separateValue
             ]

    let explicitEntry =
      skipChar '?'
        >>. pseparate
        >>. (implicitEntry <|>% (Empty, Empty))
        //<!> "map-explicit-key"
      
    let pentry = 
      (explicitEntry <|> implicitEntry) .>> opt pseparate 
      <!> "flow-map-entry"

    let map =
      withContext FlowIn <| sepEndBy pentry psep
      <!> "flow-map"

  pmappingRef := 
    between (skipChar '{') (skipChar '}') (opt pseparate >>. Mapping.map)
      |>> (Map.ofList >> Mapping)
                  
  let mapping = pmapping

  let jsonContent = choice [ sequence
                             mapping
                             Scalars.jsonParser
                           ]

  let private flowPair =
    choice [ (withContext FlowKey Scalars.yamlParser) .>>? opt pseparate .>>.? Mapping.separateValue
             (withContext FlowKey jsonContent) .>>? opt pseparate .>>.? Mapping.adjacentValue
             preturn Empty .>>.? Mapping.separateValue
           ]
           <!> "flow-pair"

  flowPairParserRef :=
    (Mapping.explicitEntry <|> flowPair)
    |>> (Array.singleton >> Map.ofArray >> Mapping)

  let parser = sequence <|> mapping <!> "flow-collections"

flowParserRef := Collections.parser <|> Scalars.parser <!> "flow"

let parser = flowParser