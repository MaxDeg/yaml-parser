module YamlParser.BlockStyle.Collection.Sequence.Tests

open YamlParser.Types
open YamlParser.BlockStyle

open Expecto
open Expecto.Flip

open Prelude


[<Tests>]
let tests = testList "block-sequence"
              [ test "simple number sequence" {
                  testParser parser @"- 1
- 2"
                  |> succeed 
                      (Expect.equal
                        "number list must be parsed properly"
                        (Sequence [ Decimal 1.0m
                                    Decimal 2.0m
                                  ]))
                }

                test "multi type sequence with item leading space" {
                  testParser parser @"- 1
-   true
- 3"
                  |> succeed
                      (Expect.equal
                        "multi-type sequence"
                        (Sequence [ Decimal 1.0m
                                    Boolean true
                                    Decimal 3.0m
                                  ]))
                }

                test "number sequence with initial indentation" {
                  testParser parser @"   - 1
   - 2"
                  |> succeed
                      (Expect.equal
                        "number list must be parsed properly"
                        (Sequence [ Decimal 1.0m
                                    Decimal 2.0m
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
                                    Decimal -123m
                                    String "http://example.com/foo#bar"
                                  ]))
                }
              ]