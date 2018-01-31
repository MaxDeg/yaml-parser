module YamlParser.BlockStyle.Collection.Mapping.Tests

open YamlParser.Types
open YamlParser

open Expecto
open Expecto.Flip

open Prelude

[<Tests>]
let tests = testList "block-mapping"
              [ test "simple block mapping" {
                  Parser.run @"block mapping:
 key: value"
                  |> succeed 
                      (Expect.equal
                        "simple implicit key mapping"
                        (Mapping
                          [ String "block mapping", 
                            Mapping 
                              [ String "key", String "value"
                              ] 
                          ]))
                }

                test "explicit key with empty value" {
                  Parser.run @"? explicit key"
                  |> succeed
                      (Expect.equal
                        "empty value node parsed properly"
                        (Mapping
                            [ String "explicit key", Empty
                            ]))
                }

                test "explicit key with seq value" {
                  Parser.run @"? 'block key'
: - one # Explicit compact
  - two # block value
"
                  |> succeed
                      (Expect.equal
                        "explicit key with sequence value parsed properly"
                        (Mapping
                            [ String "block key",
                              Sequence [ String "one"
                                         String "two"
                                       ]
                            ]))
                }

                test "implicit keys" {
                  Parser.run @"plain key: in-line value
: # Both empty
""quoted key"":
- entry
"
                  |> succeed
                      (Expect.equal
                        "all implicit key parsed properly"
                         (Mapping
                            [ String "plain key", String "in-line value"
                              Empty, Empty
                              String "quoted key",
                              Sequence [ String "entry" ]
                            ]))
                }

                test "compact block mapping" {
                  Parser.run @"- sun: yellow
- ? earth: blue
  : moon: white"
                  |> succeed 
                      (Expect.equal
                        "compact mapping in sequence parsed properly"
                        (Sequence [ Mapping 
                                      [ String "sun", String "yellow" ]
                                    Mapping 
                                      [ Mapping 
                                          [ String "earth", String "blue" ],
                                        Mapping 
                                          [ String "moon", String "white" ]
                                      ]
                                  ]))
                }
              ]