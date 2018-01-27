#time

#load "../.paket/load/netcoreapp2.0/main.group.fsx"
#load "prelude.fs"
#load "types.fs"
#load "primitives.fs"
#load "flow-styles.fs"
#load "block-styles.fs"
#load "parser.fs"

open YamlParser
open YamlParser.Types
open FParsec

let showSpecialChars (s: string) =
  s.Replace("\t", "\\t").Replace("\n", "\\n")

Parser.run @"""implicit block key"" : [
  ""implicit flow key"" : value,
 ]"

Parser.run "\"\r\n  foo \r\n\r\n  \t bar\r\n\r\n  baz\r\n\""
|> function 
| Success(String s, _, _) -> showSpecialChars s

Parser.run "\"folded \r\nto a space,\t\r\n \r\nto a line feed, or \t\\\r\n \\ \tnon-content\""
|> function 
| Success(String s, _, _) -> showSpecialChars s


Parser.run @""" 1st non-empty

 2nd non-empty 
  3rd non-empty """

Parser.run @"plain key: in-line value
: # Both empty
""quoted key"":
- entry
"

Parser.run  @"- sun: yellow
- ? earth: blue
  : moon: white"

Parser.run "{unquoted : 'separate'}"

Parser.run "{unquoted : 'separate', http://foo.com, 42: , : omitted key,}"

Parser.run @"{
omitted value:,
http://foo.com,
? 42:,
? 43: ,
'empty':
}"

Parser.run @"{
'adjacent':value,
'readable': value,
'empty':
}"

Parser.run @"{
? explicit: entry,
implicit: entry,
?
}"

Parser.run @"{
unquoted : 'separate',
http://foo.com,
omitted value:,
: omitted key,
}"

Parser.run @"- 1
- 2"

Parser.run @"- 1
-   true
- 3"

Parser.run @"   - 1
   - 2"

Parser.run @"- - 'one' # Compact
  - 'two' # sequence"


Parser.run @"- one: two # Compact mapping"


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

Parser.run @"- [ ::vector,
  "": - ()"",
  ""Up, up and away!"",
  -123,
  http://example.com/foo#bar ]"

Parser.run @"[
# my comment
""double
 quoted"", 'single
           quoted',
   [ nested ]
]"

Parser.run @"- [ one, two, ]
- [three ,four]"

Parser.run @"[ 1, 2,
3
, 4
,
5]"


Parser.run @"[ 1,  
2
]"


Parser.run @"'quoted key':
- entry"


Parser.run @"block mapping:
 key: value"

Parser.run @"? 'block key'
: - one # Explicit compact
  - two # block value
"

Parser.run  @"[ # inline comment
# on line comment
1,
# separation comment
2    ]"

Parser.run @"[
? 'foo
 bar' : baz
]"