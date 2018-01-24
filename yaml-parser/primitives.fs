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
      (skipChar lineFeed)
  <|> (skipChar carriageReturn .>> optional (skipChar lineFeed))

let manyLineBreak = many lineBreak

let many1LineBreak = many1 lineBreak

let skipManyLineBreak = skipMany skipLineBreak

let skipMany1LineBreak = skipMany1 skipLineBreak

let whitespaces : Parser<_, State> =
  manySatisfy (fun c -> c = space || c = tabulation)

let whitespaces1 : Parser<_, State> =
  many1Satisfy (fun c -> c = space || c = tabulation)

let skipWhitespace : Parser<_, State> =
  skipSatisfy (fun c -> c = space || c = tabulation)
  
let skipManyWhitespaces : Parser<_, State> =
  skipManySatisfy (fun c -> c = space || c = tabulation)
  
let skipWhitespaces1 : Parser<_, State> =
  skipMany1Satisfy (fun c -> c = space || c = tabulation)

let pnull : Parser<_, State> = stringReturn "null" Null
let ptrue : Parser<_, State> = stringReturn "true" <| Boolean true
let pfalse : Parser<_, State> = stringReturn "false" <| Boolean false
let pnumber : Parser<_, State> = (attempt pfloat) |>> (decimal >> Decimal)

let spaceChars = [| lineFeed; carriageReturn; space; tabulation |]

let indicators = [| '-'; '?'; ':'; ','; '['; ']'; '{'; '}'
                    '#'; '&'; '*'; '!'; '|'; '>'; '\''
                    '"'; '%'; '@'; '`'
                 |]

let flowIndicators = [| ','; '['; ']'; '{'; '}' |]

(* 
  Indentation
*)
let withContext ctx (p : Parser<_, State>) =
  fun (stream : CharStream<_>) ->
    let state = stream.State
    let userState = stream.UserState
    let p' = setUserState { userState with context = ctx } >>? p
    printfn "With context %A" ctx
    let result = p' stream  
    if result.Status = Ok then
      stream.UserState <- userState
    else
      stream.BacktrackTo(state)
    printfn "Reset context %A" userState.context
      
    result

/// s-indent(n)
let indent =
  fun (stream : CharStream<_>) ->
    let state = stream.State
    let userState = stream.UserState
    let column = stream.Position.Column

    printfn "Indent' - indentation %i - %i" column userState.indent
    if column < userState.indent then
      let result = 
        skipArray (int(userState.indent - column)) skipWhitespace stream

      if result.Status = Ok then
        printfn "Indent' - Ok"
        Reply <| ()
      else
        printfn "Indent' - Error, failed to read %i white spaces" (userState.indent - column)
        stream.BacktrackTo(state)
        Reply(Error, expected 
          (sprintf "indentation of %i instead of %i" userState.indent column))
    elif column > userState.indent then
      stream.BacktrackTo(state)
      Reply(Error, expected
        (sprintf "indentation of %i instead of %i" userState.indent column))
    else
      Reply <| ()

let indentPlus1 =
  fun (stream : CharStream<_>) ->
    let userState = stream.UserState
    setUserState { userState with indent = userState.indent + 1L } stream

let indentMinus1 =
  getUserState >>= fun userState ->
    setUserState { userState with indent = userState.indent - 1L }
    <!> "Indent decrement of 1"


/// s-indent(n+m)
let indentMore (stream : CharStream<_>) =
  let state = stream.State

  skipManyWhitespaces stream |> ignore
  
  let userState = stream.UserState
  let column = stream.Position.Column

  if column > userState.indent then
    printfn "IndentMore - Ok indentation %i" column
    setUserState { userState with indent = column } stream
    |> ignore
    Reply <| ()
  else
    printfn "IndentMore - Error indentation %i > %i" column userState.indent

    stream.BacktrackTo(state)
    Reply(Error,
      expected (sprintf "indentation of %i" userState.indent))

// s-indent(m)
let indentMany (stream : CharStream<_>) =
  skipManyWhitespaces stream |> ignore
  
  let userState = stream.UserState
  let column = stream.Position.Column
  setUserState { userState with indent = column } stream


(*
  Basic Structures
*)
let lineStart =
  getPosition >>= fun pos -> 
    if pos.Column = 1L then preturn ()
    else pzero
    
let separateInLine = skipWhitespaces1 <|> lineStart

let comments = 
  let commentText =
    pstring "#"
    >>. manySatisfy (fun c -> c <> lineFeed
                           && c <> carriageReturn
                           && c <> byteOrderMark)
    //<!> "comment-text"

  let sbComment = 
    opt (separateInLine >>. opt commentText)
    .>>? (skipLineBreak <|> eof <!> "sb-comment-break")
    //<!> "sb-comment"

  let lComment =
    separateInLine
    >>? opt commentText
    .>>? skipLineBreak //<|> eof)
    <!> "l-comment"

  (sbComment <|> (lineStart >>% None)) >>. many lComment
  |>> (List.choose id >> List.map trim >> Comment)
  <!> "comment"

let flowLinePrefix = indent .>> opt separateInLine

let linePrefix =
  getUserState >>= fun { context = context } ->
    match context with
    | BlockOut | BlockIn | BlockKey ->
        indent
    | FlowOut | FlowIn | FlowKey ->
        flowLinePrefix
    //<!> "line-prefix"

let pseparate =
  getUserState >>= fun { context = ctx } ->
  match ctx with
  | BlockKey | FlowKey -> 
      separateInLine >>% Empty

  | BlockOut | BlockIn
  | FlowOut  | FlowIn ->
      (comments .>>? flowLinePrefix) <|> (separateInLine >>% Empty)
      <!> "separate"

let emptyLine : Parser<_, State> = 
  whitespaces >>. lineBreak

let folded = 
  let blFolded =
    lineBreak >>. many emptyLine
    |>> (List.map string >> String.concat "")
    <!> "b-l-folded"
  
  opt separateInLine
  >>? withContext FlowIn blFolded
  .>>? flowLinePrefix
  <!> "folded"