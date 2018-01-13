
// open System

// module Yaml =
//   type Node = Mapping of Map<Node, Node>
//             | Sequence of Node list
//             | Scalar of string
//             | ScalarFloat of float
//             | ScalarInt32 of int32
//             | ScalarInt64 of int64
//             | Comment of string
//             | Null
//             | Boolean of bool
//             | Empty

//   module Parser =
//     open FParsec

//     type State = { indent: int64 }

//     let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
//       fun stream ->
//         printfn "%A: Entering %s" stream.Position label
//         let reply = p stream
//         printfn "%A: Leaving %s (%A - %A)"
//           stream.Position
//           label
//           reply.Status
//           reply.Result
//         reply

//     let parser, parserRef = createParserForwardedToRef<Node, State>()

//     module Primitives =

//       let space = '\x20'

//       let pspace = pchar space >>% "\x20"

//       let indent n = skipArray n pspace

//       let indented n =
//         many pspace >>?
//         getPosition >>= fun pos ->
//           if pos.Column > n then
//             preturn pos.Column
//           else
//             fail (sprintf "indentation must be higher than %i" pos.Column)

//       let tab = '\x09'

//       let ptab = pchar tab >>% "\x09"

//       let lineFeed = '\x0A' 

//       let carriageReturn = '\x0D'

//       let plineBreak =
//         manySatisfy (fun c -> c = lineFeed || c = carriageReturn)

//       let pmany1LineBreak =
//         many1Satisfy (fun c -> c = lineFeed || c = carriageReturn)

//       let pwhitespaces = manyStrings (pspace <|> ptab)

//       let pwhitespaces1 = many1Strings (pspace <|> ptab)

//       let pspaceBreaks = manyStrings (plineBreak <|> pwhitespaces)

//       let comment = attempt (pstring "#") >>? pwhitespaces >>. restOfLine false |>> Comment

//       // let indented =
//       // getPosition >>= fun pos ->
//       // getUserState >>= fun state ->
//       // if pos.Column > state.indent then
//       // preturn () <!> "indentation"
//       // else
//       // fail "wrong indentation"

//       // let sameOrIndented =
//       // getPosition >>= fun pos ->
//       // getUserState >>= fun state ->
//       // if pos.Column >= state.indent then
//       // preturn () <!> "same or more indentation"
//       // else
//       // fail "wrong indentation"

//       let sameIndentation =
//         getPosition >>= fun pos ->
//         getUserState >>= fun state ->
//           if pos.Column = state.indent then
//             preturn () <!> "same or indentation"
//           else
//             fail "wrong indentation" <!> sprintf "wrong identation %i <> %i" pos.Column state.indent

//       let withPos p =
//         getPosition >>= fun pos ->
//         getUserState >>= fun state ->
//           setUserState { indent = pos.Column }
//           >>? p <!> sprintf "position > %i" pos.Column
//           .>> setUserState state

//       let block p = withPos <| many1 (sameIndentation >>. p) <!> "block"

//       [<RequireQualifiedAccess>]
//       module Scalar =
//         let parser, parserRef = createParserForwardedToRef<Node, State>()

//         let nullLiteral = stringReturn "null" Null
//         let trueLiteral = stringReturn "true" <| Boolean true
//         let falseLiteral = stringReturn "false" <| Boolean false
//         let numberLiteral =
//               (attempt pfloat |>> ScalarFloat)
//           <|> (attempt pint32 |>> ScalarInt32)
//           <|> (attempt pint64 |>> ScalarInt64)

//         module FlowStyle =
//           let multiline p = ()
//           let doubleQuoted = ()

//           let singleQuotedStringLiteral isImplicitKey =
//             let indicator = pwhitespaces >>. pstring "'" .>>. pwhitespaces
//             let escapedCharSnippet  = pstring "''" >>. preturn "'"
//             let forbiddenChars = [| '\''; lineFeed; carriageReturn |]
//             let normalCharSnippet = manySatisfy (fun c -> not <| Array.contains c forbiddenChars)

//             let singleline = stringsSepBy normalCharSnippet escapedCharSnippet
//             let multiline = stringsSepBy singleline (plineBreak .>> pwhitespaces >>% " ")

//             between indicator indicator (if isImplicitKey then singleline else multiline)
//             |>> Scalar

//           let doubleQuotedStringLiteral isImplicitKey =
//             let indicator = pwhitespaces >>. pstring "\"" .>>. pwhitespaces
//             let escape= anyOf "\"\\/bfnrt"
//                         |>> function
//                         | 'b' -> "\b"
//                         | 'f' -> "\u000C"
//                         | 'n' -> "\n"
//                         | 'r' -> "\r"
//                         | 't' -> "\t"
//                           | c -> string c // every other char is mapped to itself
//             let unicodeEscape =
//               /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
//               let hex2int c = (int c &&& 15) + (int c >>> 6) * 9

//               pstring "u"
//               >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
//                 (hex2int h3) * 4096
//                 + (hex2int h2) * 256
//                 + (hex2int h1) * 16
//                 + hex2int h0
//               |> char |> string)

//             let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape)
//             let forbiddenChars= [| '\\'; '"'; lineFeed; carriageReturn |]
//             let normalCharSnippet = manySatisfy (fun c -> not <| Array.contains c forbiddenChars)

//             let singleline = stringsSepBy normalCharSnippet escapedCharSnippet
//             let multiline = stringsSepBy singleline (plineBreak .>> pwhitespaces >>% " ")

//             between indicator indicator (if isImplicitKey then singleline else multiline)
//             |>> Scalar

//           let plainStringLiteral, flowCollectionPlainStringLiteral, implicitKeyPlainLiteral =
//             let firstChar =
//               let forbiddenFirstChars = [| '['; ']'; '{'; '}'; ',' |]
//               satisfy (fun  c -> not <| Array.contains c forbiddenFirstChars)
//               <|> (satisfy (fun c -> c = ':' || c = '?' || c = '-') .>> notFollowedBy unicodeSpaces1)
            
//             let normalCharSnippet forbiddenChars = manySatisfy (fun x -> not <| Array.contains x forbiddenChars)
//             let escapedCharSnippet = 
//               pstring ":" .>> notFollowedBy unicodeSpaces1
//               <|> (previousCharSatisfies (fun c -> c <> ' ') >>. pstring "#")

//             //notFollowedBy ((pstring ":" .>> unicodeSpaces1) <|> (unicodeSpaces1 >>. pstring "#")) >>. pzero
//             let parser forbiddenChars =
//               firstChar .>>. stringsSepBy (normalCharSnippet forbiddenChars) (attempt escapedCharSnippet)
//               >>= fun (c, s) -> sprintf "%c%s" c s |> Scalar |> preturn

//             parser [| ':'; '#' |],
//             parser [| ':'; '#'; '['; ']'; '{'; '}'; ',' |],
//             parser [| ':'; '#'; '['; ']'; '{'; '}'; ','; '\n'; '\r'; '\u0085'; '\u2028'; '\u2029' |]

//           let stringLiteral =
//             doubleQuotedStringLiteral false
//             <|> singleQuotedStringLiteral false
//             <|> plainStringLiteral
//             <|> flowCollectionPlainStringLiteral
//             <|> implicitKeyPlainLiteral

//           // module BlockStyle =
//           // // let literal = pstring "|"
//           // // let folded = pstring ">"
//           // let stringLiteral =
//           // let normalCharSnippet = manySatisfy (fun c -> c <> ':' && c <> '\\')
//           // let escapedCharSnippet = pstring ":" .>> unicodeSpaces1
//           // stringsSepBy normalCharSnippet escapedCharSnippet |>> Scalar

//         parserRef := choice [ nullLiteral
//                               trueLiteral
//                               falseLiteral
//                               numberLiteral
//                               FlowStyle.stringLiteral
//                               // BlockStyle.stringLiteral
//                             ]

//       [<RequireQualifiedAccess>]
//       module Mapping =
//         module BlockStyle =
//           let complexKey = pstring "?" .>>? pwhitespaces

//           let parser =
//             let pimplicitKey = 
//               choice  [ Scalar.nullLiteral
//                         Scalar.trueLiteral
//                         Scalar.falseLiteral
//                         Scalar.numberLiteral
//                         Scalar.FlowStyle.singleQuotedStringLiteral true
//                         Scalar.FlowStyle.doubleQuotedStringLiteral true
//                         Scalar.FlowStyle.implicitKeyPlainLiteral
//                       ]
//               .>>? pwhitespaces
//               .>>? pstring ":"
//               .>>? (pwhitespaces1 <|> (pwhitespaces .>> pmany1LineBreak >>. pwhitespaces))
//               <!> "map-separator"

//             block (
//               pimplicitKey
//               .>>. parser
//               .>> plineBreak
//               .>> pwhitespaces
//               <!> "map-item"
//             ) <!> "map"
//             |>> (Map.ofList >> Mapping)

//       [<RequireQualifiedAccess>]

//       module Sequence =
//         module BlockStyle =
//           let parser =
//             block (
//               pstring "-" 
//               >>? pwhitespaces1
//               >>. parser
//               .>> plineBreak
//               .>> pwhitespaces
//               <!> "seq-item"
//             ) <!> "seq"
//             |>> Sequence

//     parserRef := choice [ Primitives.comment
//                           Primitives.Sequence.BlockStyle.parser
//                           Primitives.Mapping.BlockStyle.parser
//                           Primitives.Scalar.nullLiteral
//                           Primitives.Scalar.trueLiteral
//                           Primitives.Scalar.falseLiteral
//                           Primitives.Scalar.numberLiteral
//                           Primitives.Scalar.FlowStyle.stringLiteral
//                         ] <!> "value"

//     let yaml = Primitives.pwhitespaces >>. parser .>> Primitives.pwhitespaces .>> eof

// open Yaml
// open Yaml.Parser
// open FParsec

// runParserOnString Yaml.Parser.yaml { indent = 1L } "" @"- 1
// - 2
// - - 3.1
//   - 3.2"


// runParserOnString Yaml.Parser.yaml { indent = 1L } "" @"""key"":
// - 1
// - 2"


// runParserOnString Yaml.Parser.yaml { indent = 1L } "" @"'key':
// - 1
// - 2
// - - 3.1
//   - 3.2
// - 4.1: 1
//   4.2: 2
// 'key2': true"
