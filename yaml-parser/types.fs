module YamlParser.Types

open FParsec

type Node = Mapping of Map<Node, Node>
          | Sequence of Node list
          | Scalar of string
          | ScalarFloat of float
          | ScalarInt32 of int32
          | ScalarInt64 of int64
          | Comment of string
          | Null
          | Boolean of bool
          | Empty

type Context = BlockKey
             | BlockIn
             | BlockOut
             | FlowKey
             | FlowIn
             | FlowOut

type Chomping = Strip
              | Clip
              | Keep

type Indentation = int64

type IndentParser<'a, 'state> = Context -> Indentation -> Parser<'a, 'state>
