module YamlParser.FlowStyle

open Prelude

open YamlParser.Types
open YamlParser.Primitives

open FParsec

let flowParser, flowParserRef = createForwardParserRef()

module Scalars =
  let plain ctx indent =
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
            (pstring ":" .>> notFollowedBy whitespaces1)
        <|> (pstring "#" .>> previousCharSatisfies (fun c -> c <> space && c <> tabulation))
      
      stringsSepBy normalChars exceptionChars <!> "accepted-char"
    
    let safeChar ctx = 
          whitespaces
      >>? followedBy firstChar
      >>. (match ctx with
          | FlowIn
          | FlowKey
          | BlockKey  -> acceptedChars [| '['; ']'; '{'; '}'; ',' |] .>> whitespaces

          | FlowOut
          | BlockIn
          | BlockOut  -> acceptedChars [||] .>> whitespaces)
      <!> "safe-char"
        
    match ctx with
    | BlockIn
    | BlockOut
    | FlowIn
    | FlowOut   ->  indentation indent >>= fun indent ->
                      stringsSepBy1 (safeChar ctx) ((many1LineBreak |>> (List.map string >> String.concat "")) .>>? checkIndentation indent)
                      <!> "multi-line"

    | BlockKey
    | FlowKey   ->  safeChar ctx <!> "single-line"
    <!> "plain"
    |>> String

  let doubleQuoted ctx indent = 
    between
      (skipChar '"')
      (skipChar '"')
      (manySatisfy (fun c -> c <> '"'))
    <!> "double-quoted"
    |>> String
    
  let singleQuoted ctx indent = 
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

  let parser ctx indent = choice [ pnull
                                   ptrue
                                   pfalse
                                   pnumber
                                   singleQuoted ctx indent
                                   doubleQuoted ctx indent
                                   plain ctx indent
                                 ] <!> "scalar"


module Collections = 
  let psep = 
        skipManyLineBreak
    >>? skipWhitespaces
    >>? skipChar ','
    .>> skipWhitespaces
    .>> skipManyLineBreak

  let psep' ctx indent=
    skipChar ',' .>> opt (pseparate ctx indent) <!> "flow-sep"

    
  let sequence ctx indent =
    let pitem ctx = 
          flowParser ctx indent
      .>> opt (pseparate ctx indent)
      <!> "flow-seq-item"
    
    between
      (skipChar '[')
      (skipChar ']')
      (opt (pseparate ctx indent) >>. sepEndBy (pitem FlowIn) (psep' FlowIn indent))
    |>> Sequence

  // let mapping _ indent =
  //   let pitem = 
  //     flowParser FlowIn indent <!> "flow-map-item"
    
  //   between
  //     (whitespaces .>> skipChar '{' .>> whitespaces .>> skipLineBreak)
  //     (skipLineBreak >>. whitespaces >>. skipChar '}')
  //     (sepEndBy pitem psep)
  //   |>> (Map.ofList >> Mapping)


  let parser ctx indent = sequence ctx indent
                       //<|> mapping ctx indent
                       <!> "flow-collections"

flowParserRef := fun ctx indent ->
      Collections.parser ctx indent
  <|> Scalars.parser ctx indent

let parser ctx indent = flowParser ctx indent