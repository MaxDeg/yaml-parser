module YamlParser.BlockStyle.Collection.Mapping.Tests

open YamlParser.Types
open YamlParser.BlockStyle

open Expecto
open Expecto.Flip

open Prelude

let parser = Collections.parser

[<Tests>]
let tests = testList "block-mapping"
              [ test "simple block mapping" {
                  testParser parser @"block mapping:
 key: value"
                  |> succeed 
                      (Expect.equal
                        "simple implicit key mapping"
                        (Mapping <| Map.ofList
                          [ String "block mapping", 
                            Mapping <| Map.ofList 
                              [ String "key", String "value"
                              ] 
                          ]))
                }

                test "explicit key with empty value" {
                  testParser parser @"? explicit key"
                  |> succeed
                      (Expect.equal
                        "empty value node parsed properly"
                        (Mapping <| Map.ofList
                            [ String "explicit key", Empty
                            ]))
                }

                test "explicit key with seq value" {
                  testParser parser @"? 'block key'
: - one # Explicit compact
  - two # block value
"
                  |> succeed
                      (Expect.equal
                        "explicit key with sequence value parsed properly"
                        (Mapping <| Map.ofList
                            [ String "block key",
                              Sequence [ String "one # Explicit compact"
                                         String "two # block value"
                                       ]
                            ]))
                }

                test "implicit keys" {
                  testParser parser @"plain key: in-line value
: # Both empty
""quoted key"":
- entry
"
                  |> succeed
                      (Expect.equal
                        "all implicit key parsed properly"
                         (Mapping <| Map.ofList
                            [ String "plain key", String "in-line value"
                              Empty, Empty
                              String "quoted key",
                              Sequence [ String "entry" ]
                            ]))
                }

                test "compact block mapping" {
                  testParser parser @"- sun: yellow
- ? earth: blue
  : moon: white"
                  |> succeed 
                      (Expect.equal
                        "compact mapping in sequence parsed properly"
                        (Sequence [ Mapping <| Map.ofList 
                                      [ String "sun", String "yellow" ]
                                    Mapping <| Map.ofList 
                                      [ Mapping <| Map.ofList 
                                          [ String "earth", String "blue" ],
                                        Mapping <| Map.ofList 
                                          [ String "moon", String "white" ]
                                      ]
                                  ]))
                }
              ]