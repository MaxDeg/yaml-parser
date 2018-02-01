#time

#load "../.paket/load/netcoreapp2.0/main.group.fsx"
#load "prelude.fs"
#load "types.fs"
#load "primitives.fs"
#load "flow-styles.fs"
#load "block-styles.fs"
#load "parser.fs"

open YamlParser

let showSpecialChars (s: string) =
  s.Replace("\t", "\\t").Replace("\n", "\\n")

// unit test
Parser.run ">\r\n folded\r\n text\r\n\r\n"

Parser.run ">1+\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment"

Parser.run ">+1\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment"


Parser.run ">-\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment"

// unit test

Parser.run @"
PatchOperation:
  path:
      $ref: '#/definitions/String'
      description: 'The <code>op</code> operation ''s target, as identified by a <a href=""https://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-08"">JSON Pointer</a> value that references a location within the targeted resource. For example, if the target resource has an updateable property of <code>{""name"":""value""}</code>, the path for this property is <code>/name</code>. If the <code>name</code> property value is a JSON object (e.g., <code>{""name"": {""child/name"": ""child-value""}}</code>), the path for the <code>child/name</code> property will be <code>/name/child~1name</code>. Any slash (""/"") character appearing in path names must be escaped with ""~1"", as shown in the example above. Each <code>op</code> operation can have only one <code>path</code> associated with it.'
"


Parser.run @"
PatchOperation:
  description: 'A single patch operation to apply to the specified resource. Please refer to http://tools.ietf.org/html/rfc6902#section-4 for an explanation of how each operation is used.'
  properties:
    from:
      $ref: '#/definitions/String'
      description: 'The <code>copy</code> update operation''s source as identified by a <code>JSON-Pointer</code> value referencing the location within the targeted resource to copy the value from. For example, to promote a canary deployment, you copy the canary deployment ID to the affiliated deployment ID by calling a PATCH request on a <a>Stage</a> resource with <code>""op"":""copy""</code>, <code>""from"":""/canarySettings/deploymentId""</code> and <code>""path"":""/deploymentId""</code>.'
    op:
      $ref: '#/definitions/Op'
      description: ' An update operation to be performed with this PATCH request. The valid value can be <code>add</code>, <code>remove</code>, <code>replace</code> or <code>copy</code>. Not all valid operations are supported for a given resource. Support of the operations depends on specific operational contexts. Attempts to apply an unsupported operation on a resource will return an error message.'
    path:
      $ref: '#/definitions/String'
      description: 'The <code>op</code> operation''s target, as identified by a <a href=""https://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-08"">JSON Pointer</a> value that references a location within the targeted resource. For example, if the target resource has an updateable property of <code>{""name"":""value""}</code>, the path for this property is <code>/name</code>. If the <code>name</code> property value is a JSON object (e.g., <code>{""name"": {""child/name"": ""child-value""}}</code>), the path for the <code>child/name</code> property will be <code>/name/child~1name</code>. Any slash (""/"") character appearing in path names must be escaped with ""~1"", as shown in the example above. Each <code>op</code> operation can have only one <code>path</code> associated with it.'
    value:
      $ref: '#/definitions/String'
      description: 'The new target value of the update operation. It is applicable for the <code>add</code> or <code>replace</code> operation. When using AWS CLI to update a property of a JSON value, enclose the JSON object with a pair of single quotes in a Linux shell, e.g., ''{""a"": ...}''. In a Windows shell, see <a href=""http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json"">Using JSON for Parameters</a>.'
  type: object
"



showSpecialChars @">

 folded
 line

 next
 line
   * bullet

   * list
   * lines

 last
 line

# Comment"


Parser.run @"- | # Empty header
 literal
  folded
 keep
  strip"

Parser.run "| # Empty header\r\n literal"

Parser.run "|5 # Indentation indicator\r\n     folded"

Parser.run "|+ # Chomping indicator\r\nkeep\r\n"


Parser.run "|1 # Both indicatorsr\r\n strip"

Parser.run "|\r\n literal\r\n \ttext\r\n"

Parser.run "\"they’re \\\" planning to travel.\""

Parser.run "\"one tab \\t later\""
Parser.run "#only a comment"


Parser.run @"""implicit block key"" : [
  ""implicit flow key"" : value,
 ]"

Parser.run "\"\r\n  foo \r\n\r\n  \t bar\r\n\r\n  baz\r\n\""

Parser.run "\"folded \r\nto a space,\t\r\n \r\nto a line feed, or \t\\\r\n \\ \tnon-content\""


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