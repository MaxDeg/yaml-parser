module YamlParser.Primitives

open Prelude

open YamlParser.Types

open FParsec


let parser, parserRef = createParserForwardedToRef<Value, State>()

(*
  Debugging/Tracing operator
*)
let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
#if INTERACTIVE
  fun stream ->
    printfn "%A: Entering %s" stream.Position label
    let reply = p stream
    printfn "%A: Leaving %s (%A - %A)"
      stream.Position
      label
      reply.Status
      reply.Result
    reply
#else
  p
#endif


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

let skipManyLineBreak = skipMany skipLineBreak

let skipMany1LineBreak = skipMany1 skipLineBreak

let whitespace : Parser<_, State> =
  satisfy (fun c -> c = space || c = tabulation)

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
    //printfn "With context %A" ctx
    let result = p' stream  
    if result.Status = Ok then
      stream.UserState <- userState
    else
      stream.BacktrackTo(state)
    //printfn "Reset context %A" userState.context
      
    result

/// s-indent(n)
let indent =
  fun (stream : CharStream<_>) ->
    let state = stream.State
    let userState = stream.UserState
    let column = stream.Position.Column

    // printfn "Indent' - indentation %i - %i" column userState.indent
    if column < userState.indent then
      let result = 
        skipArray (int(userState.indent - column)) skipWhitespace stream

      if result.Status = Ok then
        // printfn "Indent' - Ok"
        Reply <| ()
      else
        // printfn "Indent' - Error, failed to read %i white spaces" (userState.indent - column)
        stream.BacktrackTo(state)
        Reply(Error, expected 
          (sprintf "indentation of %i instead of %i" userState.indent column))
    elif column > userState.indent then
      stream.BacktrackTo(state)
      Reply(Error, expected
        (sprintf "indentation of %i instead of %i" userState.indent column))
    else
      Reply <| ()

let indentPlus1 p =
  getUserState >>= fun userState ->
    setUserState { userState with indent = userState.indent + 1L }
    >>? p
    .>> setUserState { userState with indent = userState.indent }
    <!> sprintf "indentPlus1: %i" userState.indent

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
    // printfn "IndentMore - Ok indentation %i" column
    setUserState { userState with indent = column } stream
    |> ignore
    Reply <| ()
  else
    // printfn "IndentMore - Error indentation %i > %i" column userState.indent

    stream.BacktrackTo(state)
    Reply(Error,
      expected (sprintf "indentation of %i" userState.indent))

// s-indent(m)
let indentMany (stream : CharStream<_>) =
  skipManyWhitespaces stream |> ignore
  
  let userState = stream.UserState
  let column = stream.Position.Column
  setUserState { userState with indent = column } stream

let lessIndent =
  getUserState >>= fun { indent = indent } ->
  getPosition >>= fun pos ->
    // Column 1 is the lowest possible
    if pos.Column = 1L || pos.Column < indent then
      preturn ()
    else
      pzero <!> sprintf "indentation should be less than %i" pos.Column

(*
  Basic Structures
*)
let lineStart =
  getPosition >>= fun pos -> 
    if pos.Column = 1L then preturn ()
    else pzero
    
let separateInLine = skipWhitespaces1 <|> lineStart

let commentText =
  pstring "#"
  >>. manySatisfy (fun c -> c <> lineFeed
                         && c <> carriageReturn
                         && c <> byteOrderMark)
  <!> "comment-text"

let comment, comments = 
  let sbComment = 
    opt (separateInLine >>. opt commentText)
    .>>? (skipLineBreak <|> eof)
    <!> "sb-comment"

  let lComment =
    separateInLine
    >>? opt commentText
    .>>? skipLineBreak
    <!> "l-comment"

  // single comment
  (sbComment |>> function Some(Some c) -> Some <| Comment [ c ] | _ -> None),
  //  multi comments
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
      separateInLine >>% None

  | BlockOut | BlockIn
  | FlowOut  | FlowIn ->
      (comments .>>? flowLinePrefix |>> Some) <|> (separateInLine >>% None)
      <!> "separate"

let emptyLine : Parser<_, State> = 
  (linePrefix <|> (skipManyWhitespaces >>? lessIndent))
  >>? lineBreak <!> "empty-line"

let folded =
  (lineBreak >>? many1 emptyLine
  |>> (List.map string >> String.concat ""))
  <|> (lineBreak >>% " ")
  <!> "b-l-folded"

let flowFolded = 
  opt separateInLine
  >>? withContext FlowIn folded
  .>>? flowLinePrefix
  <!> "folded"