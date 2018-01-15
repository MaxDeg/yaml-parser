module YamlParser.FlowStyle.Collection.Sequence.Tests

open YamlParser.Types
open YamlParser.FlowStyle

open Expecto
open Expecto.Flip

open Prelude

let parser = Collections.parser

[<Tests>]
let tests = testList "flow-sequence"
              [ test "multi-kind items" {
                  testParser parser @"[
""double quoted"", 'single quoted',
   [ nested ]
]"
                  |> succeed
                      (Expect.equal 
                        "Correct sequence read"
                        (Sequence [ String "double quoted"
                                    String "single quoted"
                                    Sequence [ String "nested " ]
                                  ]))
                }

                test "with comments inside" {
                  testParser parser @"[ # inline comment
# on line comment
1,
# separation comment
2    ]"
                  |> succeed
                      (Expect.equal
                        "comments in list properly parsed"
                        (Sequence [ Decimal 1.0m
                                    Decimal 2.0m
                                  ]))
                }

                test "playing with ','" {
                  testParser parser "[ 'one', two, ]"
                  |> succeed
                      (Expect.equal
                        "final ',' is properly parsed"
                        (Sequence [ (String "one")
                                    (String "two")
                                  ]))
                    
                  testParser parser "['three'   ,'four']"
                  |> succeed
                      (Expect.equal
                        "spaces before ',' properly parsed"
                        (Sequence [ (String "three")
                                    (String "four")
                                  ]))
                                      
                  testParser parser @"[ 1, 2,
3
, 4
,
5]"
                  |> succeed 
                      (Expect.equal
                        "multiline ',' parsed properly"
                        (Sequence [ Decimal 1.0m
                                    Decimal 2.0m
                                    Decimal 3.0m
                                    Decimal 4.0m
                                    Decimal 5.0m
                                  ]))
                }
              ]