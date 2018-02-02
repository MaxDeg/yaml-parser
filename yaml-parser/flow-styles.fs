module YamlParser.FlowStyle

open Prelude

open System

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
        (attempt <| pipe3 flowFolded plainChar plainLine (fun a b c -> a + b + c))
      <!> "plain-next-lines"
    
    let tryParse = function
    | InvariantEqual "null" -> Null
    | InvariantEqual "true" -> Boolean true
    | InvariantEqual "false" -> Boolean false
    | IsDecimal d -> Decimal d
    | s -> String s

    getUserState >>= fun { context = ctx } ->
      match ctx with
      | FlowOut | FlowIn
      | BlockOut | BlockIn ->
          pipe2 plainOneLine plainNextLines (+)
          |>> tryParse

      | BlockKey | FlowKey ->
          plainOneLine |>> tryParse    

  let private jsonChar c = 
    c = '\x09' || not (Char.IsControl c)
    
  let private jsonNonSpaceChar c = 
    c <> space && c <> tabulation && not (Char.IsControl c)

  let doubleQuoted =
    let escapedChar =
      let chars =
        [| '0'; 'a'; 'b'; 't'; 'n'; 'v'; 'f'; 'r'
           'e'; '\t'; ' '; '"'; '/'; '\\'; 'N'
           '_'; 'L'; 'P'; 'x'; 'u'; 'U'
        |]

      pstring "\\" >>? anyOf chars 
      |>> function
      | '0' -> "\0"
      | 'a' -> "\a"
      | 'b' -> "\b"
      | 't' -> "\t"
      | 'n' -> "\n"
      | 'v' -> "\v"
      | 'f' -> "\f"
      | 'r' -> "\r"
      | 'e' -> "\e"
      | '\t' -> "\\t"
      | 'N' -> "\N"
      | 'L' -> "\L"
      | 'P' -> "\P"
      | 'x' -> "\x"
      | 'u' -> "\u"
      | 'U' -> "\U"
      | c -> string c

    let nonSpaceChar =
      (escapedChar
      <|> many1Satisfy (fun c -> c <> '\\' && c <> '"' && jsonNonSpaceChar c))

    let oneLine =
      manyStrings
        (escapedChar
        <|> many1Satisfy (fun c -> c <> '\\' && c <> '"' && jsonChar c))

    let inLine =
      let s = nonSpaceChar
      manyStrings <| attempt (pipe2 whitespaces (many1Strings s) (+))

    let nextLine =
      let pbreak =
        attempt (pipe5
          whitespaces
          (pstring "\\") // ignore
          lineBreak // ignore
          (withContext FlowIn (manyChars emptyLine))
          flowLinePrefix
          (fun a _ _ d _ -> a + d))
        <|> flowFolded

      let p =
        (pbreak .>>.? opt (pipe2 nonSpaceChar inLine (+)))
        |>> fun (a, b) -> a + Option.defaultValue "" b
      
      pipe2 (many1Strings p) whitespaces (+)

    let text =
      function
      | FlowOut | FlowIn
      | BlockOut | BlockIn ->
          pipe2 inLine (nextLine <|> whitespaces) (+)
      | FlowKey | BlockKey ->
          oneLine
    
    getUserState >>= fun { context = ctx } ->
      between
        (skipChar '"')
        (skipChar '"')
        (text ctx)
      <!> "double-quoted"
      |>> String
    
  let singleQuoted = 
    let escapedChar = pstring "''" >>% "'"
    let nonSpaceChar = 
      (escapedChar
      <|> many1Satisfy (fun c -> c <> '\'' && jsonNonSpaceChar c))

    let oneLine =
      manyStrings
        (escapedChar <|> many1Satisfy (fun c -> c <> '\'' && jsonChar c))

    let inLine =
      manyStrings
        (whitespaces .>>.?
        ((many1Satisfy (fun c -> c <> '\'' && jsonNonSpaceChar c))
        <|> escapedChar)
        |>> fun (a, b) -> a + b)
      <!> "singleline"

    let multiLine =
      let nextLine =
        pipe2
          flowFolded
          (opt (pipe2 nonSpaceChar inLine (+)))
          (fun a b -> a + Option.defaultValue "" b)
        <!> "single-quoted-nextline"
      
      pipe2 inLine (manyStrings nextLine) (+)
    
    getUserState >>= fun { context = ctx } ->
      let text = 
        match ctx with
        | FlowIn   | FlowOut
        | BlockIn  | BlockOut -> 
            pipe2 multiLine whitespaces (+)

        | BlockKey | FlowKey -> 
            oneLine

      let skipSingleQuote = skipChar '\''

      between skipSingleQuote skipSingleQuote text
      <!> "single-quoted"
      |>> String

  let jsonParser = singleQuoted <|> doubleQuoted <!> "json-parser"
  
  let parser = jsonParser <|> plain <!> "scalar"


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

  [<AutoOpen>]
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
               Scalars.plain .>>.? (opt pseparate >>? separateValue <|>% Empty)
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
    between (skipChar '{') (skipChar '}') (opt pseparate >>. map)
    |>> Mapping
                  
  let mapping = pmapping

  let jsonContent = choice [ sequence
                             mapping
                             Scalars.jsonParser
                           ]

  let private flowPair =
    choice [ (withContext FlowKey Scalars.plain) .>>? opt pseparate .>>.? separateValue
             (withContext FlowKey jsonContent) .>>? opt pseparate .>>.? adjacentValue
             preturn Empty .>>.? separateValue
           ]
           <!> "flow-pair"

  flowPairParserRef :=
    (explicitEntry <|> flowPair)
    |>> (Array.singleton >> Mapping)

  let parser = sequence <|> mapping <!> "flow-collections"

flowParserRef := Collections.parser <|> Scalars.parser <!> "flow"

let parser = flowParser