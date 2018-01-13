module YamlParser.Primitives

open Prelude

open YamlParser.Types

open FParsec


let createForwardParserRef() =
  let pdummy : IndentParser<Value> =
    fun _ _ _ ->
      failwith "a parser created with createParserForwardedToRef is not initialized"
  
  let r : IndentParser<Value> ref = ref pdummy

  (fun ctx indent stream -> !r ctx indent stream), r

let parser, parserRef = createForwardParserRef()

(*
  Debugging/Tracing operator
*)
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
  fun stream ->
#if INTERACTIVE
    printfn "%A: Entering %s" stream.Position label
#endif
    let reply = p stream
#if INTERACTIVE
    printfn "%A: Leaving %s (%A - %A)"
      stream.Position
      label
      reply.Status
      reply.Result
#endif
    reply


let space = '\x20'
let tabulation = '\x09'
let lineFeed = '\x0A'
let carriageReturn = '\x0D'
let byteOrderMark = '\uFEFF'

let lineBreak : Parser<_, unit> = 
      (pchar lineFeed)
  <|> (pchar carriageReturn .>> opt (pchar lineFeed) >>% lineFeed)
  
let skipLineBreak : Parser<_, unit> = 
      (skipChar lineFeed <!> "lf")
  <|> (skipChar carriageReturn .>> optional (skipChar lineFeed))    
  >>% lineFeed

let manyLineBreak = many lineBreak

let many1LineBreak = many1 lineBreak

let skipManyLineBreak = skipMany skipLineBreak

let skipMany1LineBreak = skipMany1 skipLineBreak

let whitespaces : Parser<_, unit> =
  manySatisfy (fun c -> c = space || c = tabulation)

let whitespaces1 : Parser<_, unit> =
  many1Satisfy (fun c -> c = space || c = tabulation)
  
let skipWhitespaces : Parser<_, unit> =
  skipManySatisfy (fun c -> c = space || c = tabulation)
  
let skipWhitespaces1 : Parser<_, unit> =
  skipMany1Satisfy (fun c -> c = space || c = tabulation)

let lineStart : Parser<_, unit> =
  getPosition >>= fun pos -> 
    if pos.Column = 1L then preturn ()
    else pzero
    
let separateInLine = skipWhitespaces1 <|> lineStart

let pnull : Parser<_, unit> = stringReturn "null" Null
let ptrue : Parser<_, unit> = stringReturn "true" <| Boolean true
let pfalse : Parser<_, unit> = stringReturn "false" <| Boolean false
let pnumber : Parser<_, unit> = (attempt pfloat) |>> (decimal >> Decimal)

let comments = 
  let commentText =   pstring "#"
                  >>. manySatisfy (fun c -> c <> lineFeed && c <> carriageReturn && c <> byteOrderMark)

  let sbComment = opt (separateInLine >>? opt commentText) .>>? many1LineBreak <!> "sb-comment"
  let lComment = separateInLine >>? opt commentText .>>? many1LineBreak <!> "l-comment"

  (sbComment <|> (lineStart >>% None)) >>? many lComment
  |>> (List.choose id >> List.map (trim >> Comment))
  <!> "comment"

let pseparate ctx indent =
  match ctx with
  | BlockKey | FlowKey  -> separateInLine >>% []
  | BlockOut | BlockIn
  | FlowOut  | FlowIn   -> comments .>> whitespaces <|> (separateInLine >>% [])

let empty : Parser<_, unit> = preturn Empty

let checkIndentation indent =
  getPosition >>= fun pos ->
    if pos.Column = indent then
      preturn ()
    else
      pzero <!> sprintf "wrong identation %i <> %i" pos.Column indent

let withPos p =
  getPosition >>= fun pos ->
    p pos.Column <!> sprintf "position > %i" pos.Column

let indentation minimalIndent = 
  whitespaces
  >>. getPosition >>= fun pos ->
    if pos.Column >= minimalIndent then
      preturn pos.Column
    else
      pzero <!> sprintf "minimal indentation of %i required" minimalIndent
