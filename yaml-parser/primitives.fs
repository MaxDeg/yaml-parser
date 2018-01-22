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
    printfn "%A[%A]: Leaving %s (%A - %A)"
      stream.Position
      stream.UserState.context
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
  
let skipWhitespaces : Parser<_, State> =
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
let checkIndentation =
  //whitespaces >>.
  getUserState >>= fun { indent = indent } ->
  getPosition >>= fun pos ->
    if pos.Column - 1L = indent then
      preturn () <!> sprintf "indentation: %i" indent
    else
      pzero <!> sprintf "wrong identation %i <> %i" pos.Column indent

let withContext ctx (p : Parser<_, State>) =
  fun (stream : CharStream<_>) ->
    let state = stream.State
    let userState = stream.UserState
    let p' = setUserState { userState with context = ctx } >>? p

    let result = p' stream  
    if result.Status = Ok then
      stream.UserState <- userState
    else
      stream.BacktrackTo(state)
      
    result

let indent x p =
  fun (stream : CharStream<_>) ->
    let state = stream.State
    let userState = stream.UserState
    let pos = stream.Position

    printfn "set indentation to %i" (pos.Column - 1L + x)

    let p' = 
      setUserState { userState with indent = pos.Column - 1L + x }
      >>? p
    
    let result = p' stream
    
    if result.Status = Ok then
      stream.UserState <- userState
    else
      stream.BacktrackTo(state)
    
    printfn "Reset indentation to %i" (stream.UserState.indent)  
    result

let indent1 p = indent 1L p

let indentMany p =
  whitespaces >>? indent 0L p
    
let indent1Many p =
  whitespaces1 >>? indent 0L p

/// s-indent(n)
let indent' =
  fun (stream : CharStream<_>) ->
    let state = stream.State
    let userState = stream.UserState
    let column = stream.Position.Column - 1L

    printfn "Indent' - Ok indentation %i - %i" column userState.indent
    if column <> userState.indent then
      let result = whitespaces1 stream
      if result.Status = Ok then
        let column' = column + (int64 result.Result.Length)
        if column' = userState.indent then
          setUserState { userState with indent = column' } stream
          |> ignore
          Reply <| ()
        else
          stream.BacktrackTo(state)
          Reply(Error,
            expected (sprintf "indentation of %i" userState.indent))
      else
        stream.BacktrackTo(state)
        Reply(Error,
          expected (sprintf "indentation of %i" userState.indent))
    else
      Reply <| ()

let indent1' =
  fun (stream : CharStream<_>) ->
    let state = stream.State
    let userState = stream.UserState
    let column = stream.Position.Column

    if column <> userState.indent then
      let result = whitespaces1 stream
      if result.Status = Ok then
        let column' = column + (int64 result.Result.Length)
        if column' = userState.indent then
          setUserState { userState with indent = column' } stream
          |> ignore
          Reply <| ()
        else
          stream.BacktrackTo(state)
          Reply(Error,
            expected (sprintf "indentation of %i" userState.indent))
      else
        stream.BacktrackTo(state)
        Reply(Error,
          expected (sprintf "indentation of %i" userState.indent))
    else
      Reply(Error,
        expected (sprintf "indentation of %i" userState.indent))

/// s-indent(n+m)
let indentMore =
  fun (stream : CharStream<_>) ->
    let state = stream.State

    whitespaces stream |> ignore

    let userState = stream.UserState
    let column = stream.Position.Column - 1L

    if column >= userState.indent then
      printfn "IndentMore - Ok indentation %i" column
      setUserState { userState with indent = column } stream
      |> ignore
      Reply <| ()
    else
      printfn "IndentMore - Error indentation %i <> %i" column userState.indent

      stream.BacktrackTo(state)
      Reply(Error,
        expected (sprintf "indentation of %i" userState.indent))

/// s-indent(n+1+m)
let indentMore1 =
  fun (stream : CharStream<_>) ->
    let state = stream.State

    whitespaces1 stream |> ignore

    let userState = stream.UserState
    let column = stream.Position.Column - 1L

    if column >= userState.indent then
      printfn "IndentMore1 - Ok indentation %i" column
      setUserState { userState with indent = column } stream
      |> ignore
      Reply <| ()
    else
      printfn "IndentMore1 - Error indentation %i <> %i" column userState.indent

      stream.BacktrackTo(state)
      Reply(Error,
        expected (sprintf "indentation of %i" userState.indent))


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
    opt (separateInLine >>? opt commentText)
    .>>? (skipLineBreak <|> eof <!> "sb-comment-break")
    //<!> "sb-comment"

  let lComment =
    separateInLine
    >>? opt commentText
    .>>? (skipLineBreak <|> eof)
    //<!> "l-comment"

  (sbComment <|> (lineStart >>% None)) >>. many lComment
  |>> (List.choose id >> List.map trim >> Comment)
  <!> "comment"

let linePrefix =
  getUserState >>= fun { context = context } ->
    match context with
    | BlockOut | BlockIn | BlockKey ->
        checkIndentation
    | FlowOut | FlowIn | FlowKey ->
        checkIndentation .>>? opt separateInLine
    //<!> "line-prefix"

let pseparate =
  getUserState >>= fun { context = ctx } ->
  match ctx with
  | BlockKey | FlowKey -> 
      separateInLine >>% Empty

  | BlockOut | BlockIn
  | FlowOut  | FlowIn ->
      (comments .>>? linePrefix) <|> (separateInLine >>% Empty)
      <!> "separate"

let emptyLine : Parser<_, State> = whitespaces >>. lineBreak

let folded = 
  let blFolded =
    lineBreak
    >>. many emptyLine
    |>> (List.map string >> String.concat "")
    //<!> "b-l-folded"
  
  opt separateInLine
  >>? withContext FlowIn blFolded
  .>>? linePrefix
  //<!> "folded"