#load "../.paket/load/netcoreapp2.0/main.group.fsx"
#load "prelude.fs"
#load "types.fs"
#load "primitives.fs"
#load "block-styles.fs"
#load "flow-styles.fs"
#load "parser.fs"

open FParsec

open YamlParser
open YamlParser.Types

#time

Parser.run @"- 1
-   true
- 3"

Parser.run @"   - 1
   - 2"

Parser.run @"? - 1
  - 2
: 3"

Parser.run @"? - 1
  - 2 : 3
: 4"

Parser.run @"- ::vector
- Up, up, and away!
- -123
- http://example.com/foo#bar"
