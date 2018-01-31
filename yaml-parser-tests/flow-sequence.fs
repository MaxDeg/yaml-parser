module YamlParser.FlowStyle.Collection.Sequence.Tests

open YamlParser.Types
open YamlParser

open Expecto
open Expecto.Flip

open Prelude

[<Tests>]
let tests = testList "flow-sequence"
              [ test "multi-kind items" {
                  Parser.run @"[
""double quoted"", 'single quoted',
   [ nested ]
]"
                  |> succeed
                      (Expect.equal 
                        "Correct sequence read"
                        (Sequence [ String "double quoted"
                                    String "single quoted"
                                    Sequence [ String "nested" ]
                                  ]))
                }

                test "with comments inside" {
                  Parser.run @"[ # inline comment
# on line comment
1,
# separation comment
2    ]"
                  |> succeed
                      (Expect.equal
                        "comments in list properly parsed"
                        (Sequence [ String "1"
                                    String "2"
                                  ]))
                }

                test "playing with ','" {
                  Parser.run "[ 'one', two, ]"
                  |> succeed
                      (Expect.equal
                        "final ',' is properly parsed"
                        (Sequence [ (String "one")
                                    (String "two")
                                  ]))
                    
                  Parser.run "['three'   ,'four']"
                  |> succeed
                      (Expect.equal
                        "spaces before ',' properly parsed"
                        (Sequence [ (String "three")
                                    (String "four")
                                  ]))
                                      
                  Parser.run @"[ 1, 2,
3
, 4
,
5]"
                  |> succeed 
                      (Expect.equal
                        "multiline ',' parsed properly"
                        (Sequence [ String "1"
                                    String "2"
                                    String "3"
                                    String "4"
                                    String "5"
                                  ]))
                }

                test "explicit flow-pair" {
                  Parser.run @"[
? 'foo
 bar' : baz
]"
                  |> succeed
                      (Expect.equal
                        "explicit flow-pair"
                        (Sequence [ Mapping [ String "foo bar", String "baz"] ]))
                }

                test "implicit flow-pair" {
                  Parser.run "[ YAML : separate ]"
                  |> succeed
                      (Expect.equal
                        "implicit yaml key with separate value"
                        (Sequence [ Mapping [ String "YAML", String "separate" ] ]))
                        
                  Parser.run "[ : empty key entry ]"
                  |> succeed
                      (Expect.equal
                        "empty key"
                        (Sequence [ Mapping [ Empty, String "empty key entry" ] ]))
                        
                  Parser.run "[ {JSON: like}:adjacent ]"
                  |> succeed
                      (Expect.equal
                        "json key"
                        (Sequence [ 
                          Mapping
                            [ (Mapping [ String "JSON", String "like" ]),
                              String "adjacent"
                            ]
                                  ]))
                }
              ]