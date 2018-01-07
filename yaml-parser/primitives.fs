module YamlParser.Primitives

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
    printfn "%A: Entering %s" stream.Position label
    let reply = p stream
    printfn "%A: Leaving %s (%A - %A)"
      stream.Position
      label
      reply.Status
      reply.Result
    reply


let space = '\x20'
let tabulation = '\x09'
let lineFeed = '\x0A'
let carriageReturn = '\x0D'

let lineBreak : Parser<_, unit> = 
  manySatisfy (fun c -> c = lineFeed || c = carriageReturn)

let lineBreak1 : Parser<_, unit> =
  many1Satisfy (fun c -> c = lineFeed || c = carriageReturn)

let skipLineBreak : Parser<_, unit> = 
  skipManySatisfy (fun c -> c = lineFeed || c = carriageReturn)

let skipLineBreak1 : Parser<_, unit> =
  skipMany1Satisfy (fun c -> c = lineFeed || c = carriageReturn)

let whitespaces : Parser<_, unit> =
  manySatisfy (fun c -> c = space || c = tabulation)

let whitespaces1 : Parser<_, unit> =
  many1Satisfy (fun c -> c = space || c = tabulation)
  
let skipWhitespaces1 : Parser<_, unit> =
  skipMany1Satisfy (fun c -> c = space || c = tabulation)

let spaceBreaks : Parser<_, unit> =
  manySatisfy (fun c -> c = space || c = tabulation || c = lineFeed || c = carriageReturn)

let lineStart : Parser<_, unit> =
  getPosition >>= fun pos -> 
    if pos.Column = 1L then preturn ()
    else pzero
    
let separateInLine = skipWhitespaces1 <|> lineStart

let pnull : Parser<_, unit> = stringReturn "null" Null
let ptrue : Parser<_, unit> = stringReturn "true" <| Boolean true
let pfalse : Parser<_, unit> = stringReturn "false" <| Boolean false
let pnumber : Parser<_, unit> = (attempt pfloat) |>> (decimal >> Decimal)

let comment = 
      separateInLine
  >>? pstring "#"
  >>? whitespaces
  >>. restOfLine false
  |>> Comment

let empty : Parser<_, unit> = preturn Empty

// let separate ctx indent =
//   match ctx with
//   | BlockOut | BlockIn
//   | FlowOut  | FlowIn   -> <|> separateInLine
//   | BlockKey | FlowKey  -> separateInLine


let checkIndentation indent =
  getPosition >>= fun pos ->
    if pos.Column = indent then
      preturn () //<!> "same indentation"
    else
      pzero <!> sprintf "wrong identation %i <> %i" pos.Column indent

let withPos p =
  getPosition >>= fun pos ->
    p pos.Column <!> sprintf "position > %i" pos.Column

// let block (p : IndentParser<_,_>) =
//   withPos <|
//     fun i -> many1 (checkIndentation i >>. p ) <!> "block"

let indentation minimalIndent = 
  whitespaces
  >>. getPosition >>= fun pos ->
    if pos.Column >= minimalIndent then
      preturn pos.Column
    else
      pzero <!> sprintf "minimal indentation of %i required" minimalIndent
