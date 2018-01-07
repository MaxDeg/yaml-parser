module YamlParser.Types

open FParsec

type Value = Mapping of Map<Value, Value>
           | Sequence of Value list
           | String of string
           | Boolean of bool
           | Decimal of decimal
           | Comment of string
           | Null
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

type IndentParser<'a> = Context -> Indentation -> Parser<'a, unit>
