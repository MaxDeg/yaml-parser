module YamlParser.BlockStyle

open System

open Prelude

open YamlParser.Types
open YamlParser.Primitives

open FParsec

let private blockParser, blockParserRef =
  createParserForwardedToRef<Value, State>()

module Scalars =
  let private chomping =
    fun (stream : CharStream<_>) ->
      let userState = stream.UserState
      let chomp =
        choice [ skipChar '-' >>% Strip
                 skipChar '+' >>% Keep
                 preturn Clip
               ] stream

      if chomp.Status = Ok then
        setUserState { userState with chomping = Some <| chomp.Result } stream
      else
        Reply(Error, chomp.Error)

  let private chompedLast =
    getUserState >>= fun { chomping = chomping } ->
      match chomping with
      | Some Strip ->
          skipLineBreak <|> eof >>% ""
      | Some Keep | Some Clip ->
          lineBreak |>> string
          <|> (eof >>% "")
      | None ->
          fun _ -> Reply(Error, expected "chomping not defined")

  let private chompedEmpty =
    let lessIndent =
      getUserState >>= fun { indent = indent } ->
      getPosition >>= fun pos ->
      if pos.Column < indent then
        preturn ()
      else
        pzero <!> sprintf "indentation should be less than %i" pos.Column

    let lessOrEqualIndent =
      getUserState >>= fun { indent = indent } ->
      getPosition >>= fun pos ->
      if pos.Column <= indent then
        preturn ()
      else
        pzero
        <!> sprintf "indentation should be less or equal than %i" pos.Column


    let trailComment =
      let lComment = 
        separateInLine >>? opt commentText .>>? skipLineBreak

      lessIndent >>. commentText .>> skipLineBreak .>>. many lComment
      |>> fun (a, b) -> Some a :: b 
                        |> List.choose id
                        |> List.map trim
                        |> Comment

    getUserState >>= fun { chomping = chomping } ->
      match chomping with
      | Some Strip | Some Clip ->
          skipMany (lessOrEqualIndent >>? skipLineBreak)
          >>. opt trailComment
          |>> fun c -> None, c
      | Some Keep ->
          withContext BlockIn (opt <| many1Chars emptyLine)
          .>>. opt trailComment
      | None ->
          fun _ -> Reply(Error, expected "chomping not defined")


  let private header =
    let indentation =
      fun (stream : CharStream<_>) ->
        let userState = stream.UserState
        let indent = anyOf [ '1' .. '9' ] |>> (string >> Int64.Parse)
        let result = indent stream

        if result.Status = Ok then
          setUserState { userState with 
                          indent      = result.Result + 1L
                          indentType  = Fixed  } stream
        else
          setUserState { userState with indentType = AutoDetect  } stream
    
    choice [ opt indentation .>> chomping
             chomping >>. opt indentation
           ] 
    >>. comment
    <!> "header"

  let chars c =
    c = '\x09' || not (Char.IsControl c) && c <> byteOrderMark

  let literal =
    let firstLineText =
      getUserState >>= fun { indentType = indentType } ->
        let indent' = 
          match indentType with
          | Fixed -> indent
          | AutoDetect -> indentMore

        withContext BlockIn (many emptyLine)
        >>? indent'
        >>. many1Satisfy chars
        <!> "literal-text-firstline"
    let text =
      withContext BlockIn (many emptyLine)
      >>? indent
      >>. many1Satisfy chars
      <!> "literal-text"
    let newLineText =
      attempt (pipe2 lineBreak text (fun a b -> string a + b))
      <!> "literal-text-newline"
    let multiLineText =
      manyStrings2 firstLineText newLineText
      <!> "literal-text-multiline"

    skipChar '|'
    >>? header
    >>? opt (pipe2 multiLineText chompedLast (+))
    .>>. chompedEmpty
    |>> fun (a, (b, _)) -> 
          Option.defaultValue "" a + Option.defaultValue "" b
          |> String
    <!> "literal"

  let parser : Parser<Value, State> = literal

module Collections =
  let private indentedBlockParser, indentedBlockParserRef =
    createParserForwardedToRef<Value, State>()
  
  [<AutoOpen>]
  module internal Sequence =
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

  [<AutoOpen>]
  module internal Mapping =
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
      |>> Mapping

    let compactMap =
      mapEntry .>>. many (indent >>? mapEntry)
      <!> "mapping"
      |>> fun (h, t) -> h::t |> Mapping

  indentedBlockParserRef := 
    choice [ // compact sequence
             (indentMore >>? compactSeq)
             // compact mapping
             (indentMore >>? compactMap)
             blockParser
             preturn Empty .>> comments
           ]
           <!> "indented-block"

  let parser =
    let seqSpaces =
      getUserState >>= fun state ->
      match state.context with
      | BlockOut -> 
          indentMinus1 >>? seq
      | _ ->
          seq

    comments >>? (seqSpaces <|> map) <!> "block-collection"


let flowInBlock =
  indentPlus1 (
    withContext FlowOut (pseparate >>? FlowStyle.parser))
  .>> comments
  <!> "flow in block"


blockParserRef := 
  choice [ indentPlus1 pseparate >>? Scalars.parser
           Collections.parser
           flowInBlock
         ]
  <!> "block-parser"

let parser = blockParser
