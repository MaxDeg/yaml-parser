module YamlParser.Primitives

open YamlParser.Types

open FParsec


let parser, parserRef =
  let pdummy : IndentParser<_, _> =
    fun ctx indent stream ->
      failwith "a parser created with createParserForwardedToRef is not initialized"
  
  let r : IndentParser<_, _> ref = ref pdummy

  (fun ctx indent stream -> !r ctx indent stream), r

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
let tab = '\x09'
let lineFeed = '\x0A'
let carriageReturn = '\x0D'

let lineBreak = 
  manySatisfy (fun c -> c = lineFeed || c = carriageReturn)

let lineBreak1 =
  many1Satisfy (fun c -> c = lineFeed || c = carriageReturn)

let whitespaces = manySatisfy (fun c -> c = space || c = tab)

let whitespaces1 = many1Satisfy (fun c -> c = space || c = tab)

let pnull = stringReturn "null" Null
let ptrue = stringReturn "true" <| Boolean true
let pfalse = stringReturn "false" <| Boolean false
let pnumber =
      (attempt pfloat |>> ScalarFloat)
  <|> (attempt pint32 |>> ScalarInt32)
  <|> (attempt pint64 |>> ScalarInt64)

let comment = 
      attempt (pstring "#")
  >>? whitespaces
  >>. restOfLine false
  |>> Comment


let sameIndentation indent =
  getPosition >>= fun pos ->
    if pos.Column = indent then
      preturn () <!> "same or indentation"
    else
      fail "wrong indentation" <!> sprintf "wrong identation %i <> %i" pos.Column indent

let withPos p =
  getPosition >>= fun pos ->
    p pos.Column <!> sprintf "position > %i" pos.Column

let block (p : IndentParser<_,_>) =
  withPos <|
    fun i -> many1 (sameIndentation i >>. p ) <!> "block"


