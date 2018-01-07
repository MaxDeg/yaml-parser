module YamlParser.FlowStyle

open Prelude

open YamlParser.Types
open YamlParser.Primitives

open FParsec


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
                      stringsSepBy1 (safeChar ctx) (lineBreak1 .>>? checkIndentation indent)
                      <!> "multi-line"

    | BlockKey
    | FlowKey   ->  safeChar ctx <!> "single-line"
    |>> String



  let parser ctx indent = choice [ pnull
                                   ptrue
                                   pfalse
                                   pnumber
                                   plain ctx indent
                                 ] <!> "scalar"


module Collections = 
  let parser ctx indent = fail "not implemented"

let parser ctx indent = Scalars.parser ctx indent
                    //<|> Collections.parser ctx indent