#load "../.paket/load/main.group.fsx"
#load "types.fs"
#load "primitives.fs"
#load "block-styles.fs"
#load "flow-styles.fs"

open YamlParser.Types
open YamlParser.Primitives
open YamlParser.BlockStyle
open YamlParser.FlowStyle

open FParsec

let yamlParser = whitespaces >>. YamlParser.Primitives.parser .>> whitespaces .>> eof
