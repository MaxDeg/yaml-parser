module YamlParser.Primitives

open Prelude

open YamlParser.Types

open FParsec


let parser, parserRef = createParserForwardedToRef<Value, State>()

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

let lineBreak : Parser<_, State> = 
      (pchar lineFeed)
  <|> (pchar carriageReturn .>> opt (pchar lineFeed) >>% lineFeed)
  
let skipLineBreak : Parser<_, State> = 
      (skipChar lineFeed <!> "lf")
  <|> (skipChar carriageReturn .>> optional (skipChar lineFeed))

let manyLineBreak = many lineBreak

let many1LineBreak = many1 lineBreak

let skipManyLineBreak = skipMany skipLineBreak

let skipMany1LineBreak = skipMany1 skipLineBreak

let whitespaces : Parser<_, State> =
  manySatisfy (fun c -> c = space || c = tabulation)

let whitespaces1 : Parser<_, State> =
  many1Satisfy (fun c -> c = space || c = tabulation)
  
let skipWhitespaces : Parser<_, State> =
  skipManySatisfy (fun c -> c = space || c = tabulation)
  
let skipWhitespaces1 : Parser<_, State> =
  skipMany1Satisfy (fun c -> c = space || c = tabulation)

let lineStart =
  getPosition >>= fun pos -> 
    if pos.Column = 1L then preturn ()
    else pzero
    
let separateInLine = skipWhitespaces1 <|> lineStart

let pnull : Parser<_, State> = stringReturn "null" Null
let ptrue : Parser<_, State> = stringReturn "true" <| Boolean true
let pfalse : Parser<_, State> = stringReturn "false" <| Boolean false
let pnumber : Parser<_, State> = (attempt pfloat) |>> (decimal >> Decimal)

let manyStringIn (values : #seq<char>) : Parser<string, _> =
  manySatisfy (fun c -> Seq.contains c values)

let comments = 
  let commentText =   pstring "#"
                  >>. manySatisfy (fun c -> c <> lineFeed && c <> carriageReturn && c <> byteOrderMark)

  let sbComment = opt (separateInLine >>? opt commentText) .>>? (skipLineBreak <|> eof) <!> "sb-comment"
  let lComment = separateInLine >>? opt commentText .>>? (skipLineBreak <|> eof) <!> "l-comment"

  (sbComment <|> (lineStart >>% None)) >>? many lComment
  |>> (List.choose id >> List.map trim >> Comment)
  <!> "comment"

let pseparate =
  getUserState >>= fun { context = ctx } ->
  match ctx with
  | BlockKey | FlowKey  -> separateInLine >>% Empty
  | BlockOut | BlockIn
  | FlowOut  | FlowIn   -> comments .>> whitespaces <|> (separateInLine >>% Empty)

let empty : Parser<_, State> = preturn Empty

let checkIndentation =
  whitespaces >>.
  getUserState >>= fun { indent = indent } ->
  getPosition >>= fun pos ->
    if pos.Column = indent then
      preturn ()
    else
      pzero <!> sprintf "wrong identation %i <> %i" pos.Column indent

let withSameOrHigherIndentation (p : Parser<_, State>) =
  fun stream ->
    whitespaces stream |> ignore

    let state = stream.State
    let userState = stream.UserState
    let pos = stream.Position

    printfn "Validation indentation: %i >= %i" pos.Column userState.indent
    if pos.Column >= userState.indent then
      stream.UserState <- { userState with indent = pos.Column }
      let result = p stream

      if result.Status = Ok then
        stream.UserState <- userState
      else
        stream.BacktrackTo(state)
        
      result
    else
      stream.BacktrackTo(state)
      Reply(Error, messageError <| sprintf "minimal indentation of %i required" userState.indent)

let withHigherIndentation (p : Parser<_, State>) =
  fun stream ->
    whitespaces stream |> ignore

    let state = stream.State
    let userState = stream.UserState
    let pos = stream.Position

    printfn "Validation indentation: %i > %i" pos.Column userState.indent
    if pos.Column > userState.indent then
      stream.UserState <- { userState with indent = pos.Column + 1L }
      let result = p stream

      if result.Status = Ok then
        stream.UserState <- userState
      else
        stream.BacktrackTo(state)
        
      result
    else
      stream.BacktrackTo(state)
      Reply(Error, messageError <| sprintf "minimal indentation of %i required" userState.indent)

let withContext ctx p =
  getUserState >>= fun state ->
    between
      (setUserState { state with context = ctx })
      (preturn ())
      p
    <!> sprintf "with context: %A" ctx


let indentation minimalIndent = 
  whitespaces
  >>. getPosition >>= fun pos ->
    printfn "Check indentation: %i >= %i" pos.Column minimalIndent
    if pos.Column >= minimalIndent then
      preturn pos.Column
    else
      pzero <!> sprintf "minimal indentation of %i required" minimalIndent

let withIndentation =
  whitespaces >>.
  getPosition >>= fun pos ->
  getUserState >>= fun state ->
    setUserState { state with indent = pos.Column }

let lowerIndentation =
  getUserState >>= fun { indent = indent } ->
  getPosition >>= fun pos ->
    if pos.Column < indent then
      preturn ()
    else
      pzero
