module YamlParser.FlowStyle

open Prelude

open YamlParser.Types
open YamlParser.Primitives

open FParsec

let private flowParser, flowParserRef = createParserForwardedToRef<Value, State>()

module Scalars =
  let plain =
    let firstChar = 
          noneOf "[]{},:?-"
      <|> (anyOf ":?-" .>> (notFollowedBy whitespaces1))
      <!> "firstchar"
    
    let acceptedChars charToExclude =
      let normalChars = 
        flip
          Array.contains
          [| yield '#'
             yield ':'
             yield carriageReturn
             yield lineFeed
             yield! charToExclude
          |]
        >> not
        |> manySatisfy
      
      let exceptionChars =
            (pstring ":" .>>? notFollowedBy whitespaces1)
        <|> (pstring "#" .>>? previousCharSatisfies (fun c -> c <> space && c <> tabulation))
      
      stringsSepBy normalChars exceptionChars <!> "accepted-char"
    
    let safeChar ctx = 
          whitespaces
      >>? followedBy firstChar
      >>. (match ctx with
          | FlowIn | FlowKey
          | BlockKey          -> acceptedChars [| '['; ']'; '{'; '}'; ',' |] .>> whitespaces

          | FlowOut | BlockIn
          | BlockOut          -> acceptedChars [||] .>> whitespaces)
      <!> "safe-char"
    
    getUserState >>= fun { context = ctx } ->
      match ctx with
      | BlockIn
      | BlockOut
      | FlowIn
      | FlowOut   ->  withSameOrHigherIndentation <|
                        stringsSepBy1 (safeChar ctx) ((many1LineBreak |>> (List.map string >> String.concat "")) .>>? checkIndentation)
                        <!> "multi-line"

      | BlockKey
      | FlowKey   ->  safeChar ctx <!> "single-line"
      <!> "plain"
      |>> String

  let doubleQuoted = 
    between
      (skipChar '"')
      (skipChar '"')
      (manySatisfy (fun c -> c <> '"'))
    <!> "double-quoted"
    |>> String
    
  let singleQuoted = 
    let escapedChar = pstring "''" >>% "'"
    let acceptedChar =  manySatisfy (fun c -> c <> '\'' && c > '\x20' && c <= '\u10FF')

    let folded =   (opt separateInLine)
               >>? many1 (whitespaces >>? lineBreak <!> "folded-line")
               .>> whitespaces
               <!> "folded"
               |>> fun s -> if s.Length > 1
                            then List.skip 1 s |> List.map string |> String.concat ""
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
      let text = match ctx with
                 | FlowIn   | FlowOut
                 | BlockIn  | BlockOut  -> pipe2
                                            (stringsSepBy1 singleline folded)
                                            whitespaces
                                            (+)

                 | BlockKey | FlowKey   -> stringsSepBy 
                                            (manySatisfy (fun c -> c <> '\'' && (c = '\x09' || (c >= '\x20' && c <= '\u10FF'))))
                                            escapedChar
      between
        (skipChar '\'')
        (skipChar '\'')
        text
      <!> "single-quoted"
      |>> String

  let yamlParser = choice [ pnull
                            ptrue
                            pfalse
                            pnumber
                            plain
                          ]

  let jsonParser = singleQuoted <|> doubleQuoted
  
  let parser = jsonParser <|> yamlParser <!> "scalar"


module Collections = 
  let psep =
    skipChar ',' .>> opt pseparate <!> "flow-sep"

  let sequence =
    let pitem = 
      flowParser .>> opt pseparate <!> "flow-seq-item"
    
    let seq =
      withContext FlowIn <| sepEndBy pitem psep

    between
      (skipChar '[')
      (skipChar ']')
      (opt pseparate >>. seq)
    |>> Sequence

  let private pmapping, pmappingRef = createParserForwardedToRef<Value, State>()

  let private mapping' =
    let pentry = 
      let implicitEntry =
        let separateValue =
          skipChar ':' >>? followedBy whitespaces1 >>. (pseparate >>. flowParser <|>% Empty) <!> "separate-value"
        let adjacentValue =
          skipChar ':' >>. (opt pseparate >>? flowParser <|>% Empty) <!> "adjacent-value"

        choice [ (Scalars.jsonParser <|> sequence <|> pmapping <|>% Empty) .>> opt pseparate .>>.? adjacentValue
                 Scalars.yamlParser .>>.? (opt pseparate >>? separateValue <|>% Empty)
                 preturn Empty .>>.? separateValue
               ]
               <!> "map-entry"

      let explicitEntry =
        skipChar '?' >>. pseparate >>. (implicitEntry <|>% (Empty, Empty)) <!> "map-explicit-key"
      
      (explicitEntry <|> implicitEntry) .>> opt pseparate
    
    let map =
      withContext FlowIn <| sepEndBy pentry psep
    
    between
      (skipChar '{')
      (skipChar '}')
      (opt pseparate >>. map)
    <!> "map"
    |>> (Map.ofList >> Mapping)

  pmappingRef := mapping'

  let mapping = pmapping

  let parser = sequence <|> mapping <!> "flow-collections"

flowParserRef := Collections.parser
             <|> Scalars.parser

let parser = flowParser