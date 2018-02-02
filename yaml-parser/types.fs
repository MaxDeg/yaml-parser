module YamlParser.Types


type Comment = Comment of string list

type Value = 
  | Mapping of Map<Value, Value> * comment: Comment option
  | Sequence of Value list * comment: Comment option
  | String of string
  | Boolean of bool
  | Decimal of decimal
  //| Comment of string list
  | Null
  | Empty

[<AutoOpen>]
module Value =
  let Mapping (s: #seq<Value * Value>) =
    Map.ofSeq s |> fun m -> Mapping(m, None)
  
  let Sequence (s: #seq<Value>) =
    List.ofSeq s |> fun m -> Sequence(m, None)


type Context =
  | BlockKey
  | BlockIn
  | BlockOut
  | FlowKey
  | FlowIn
  | FlowOut

type Chomping =
  | Strip
  | Clip
  | Keep

type Indentation = int64

type IndentationType = AutoDetect | Fixed

type State =
  { indent      : Indentation
    indentType  : IndentationType
    context     : Context
    chomping    : Chomping option
  }
