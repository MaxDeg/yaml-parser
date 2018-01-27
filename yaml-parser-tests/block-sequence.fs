module YamlParser.BlockStyle.Collection.Sequence.Tests

open YamlParser.Types
open YamlParser

open Expecto
open Expecto.Flip

open Prelude

let parser = Parser.bareDocument

[<Tests>]
let tests = testList "block-sequence"
              [ test "simple number sequence" {
                  testParser parser @"- 1
- 2"
                  |> succeed 
                      (Expect.equal
                        "number list must be parsed properly"
                        (Sequence [ String "1"
                                    String "2"
                                  ]))
                }

                test "multi type sequence with item leading space" {
                  testParser parser @"- 1
-   true
- 3"
                  |> succeed
                      (Expect.equal
                        "multi-type sequence"
                        (Sequence [ String "1"
                                    Boolean true
                                    String "3"
                                  ]))
                }

                test "number sequence with initial indentation" {
                  testParser parser @"   - 1
   - 2"
                  |> succeed
                      (Expect.equal
                        "number list must be parsed properly"
                        (Sequence [ String "1"
                                    String "2"
                                  ]))
                }

                test "sequence of compact sequence" {
                  testParser parser @"- - 'one' # Compact
  - 'two' # sequence"
                  |> succeed
                      (Expect.equal
                        "number list must be parsed properly"
                        (Sequence [ Sequence [ String "one"
                                               String "two"
                                             ]
                                  ]))
                }

                test "sequence of scalar types" {
                  testParser parser @"- ::vector
- Up, up, and away!
- -123
- http://example.com/foo#bar"
                  |> succeed 
                      (Expect.equal
                        "number list must be parsed properly"
                        (Sequence [ String "::vector"
                                    String "Up, up, and away!"
                                    String "-123"
                                    String "http://example.com/foo#bar"
                                  ]))
                }
              ]