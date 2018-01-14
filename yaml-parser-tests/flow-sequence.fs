module YamlParser.FlowStyle.Collection.Sequence.Tests

open YamlParser.Types
open YamlParser.FlowStyle

open Expecto
open Expecto.Flip

open Prelude

let parser = Collections.parser

let ``multi-kind items`` =
  testCase "multi-kind items" <| fun _ ->
    test parser @"[
""double quoted"", 'single quoted',
   [ nested ]
]"
    |> succeed (Expect.equal 
                  "Correct sequence read"
                  (Sequence [ (String "double quoted")
                              (String "single quoted")
                              (Sequence [ String "nested " ])
                            ]))

let ``with comments inside`` =
  testCase "with comments inside" <| fun _ ->
    test parser @"[ # inline comment
# on line comment
1,
# separation comment
2    ]"
    |> succeed (Expect.equal
                  "comments in list properly parsed"
                  (Sequence [ (Decimal 1.0m)
                              (Decimal 2.0m)
                            ]))


let ``playing with ','`` =
  testCase "playing with ','" <| fun _ ->
    test parser "[ 'one', two, ]"
    |> succeed (Expect.equal
                  "final ',' is properly parsed"
                  (Sequence [ (String "one")
                              (String "two")
                            ]))
      
    test parser "['three'   ,'four']"
    |> succeed (Expect.equal
                  "spaces before ',' properly parsed"
                  (Sequence [ (String "three")
                              (String "four")
                            ]))
                                
    test parser @"[ 1, 2,
3
, 4
,
5]"
    |> succeed (Expect.equal
                  "multiline ',' parsed properly"
                  (Sequence [ (Decimal 1.0m)
                              (Decimal 2.0m)
                              (Decimal 3.0m)
                              (Decimal 4.0m)
                              (Decimal 5.0m)
                            ])) 

[<Tests>]
let collection =
  testList "flow-sequence" [ ``multi-kind items``
                             ``with comments inside``
                             ``playing with ','``
                           ]