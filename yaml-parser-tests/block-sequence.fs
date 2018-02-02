module YamlParser.BlockStyle.Collection.Sequence.Tests

open YamlParser.Types
open YamlParser

open Expecto
open Expecto.Flip

open Prelude

[<Tests>]
let tests = testList "block-sequence"
              [ test "simple number sequence" {
                  Parser.run @"- 1
- 2"
                  |> succeed 
                      (Expect.equal
                        "number list must be parsed properly"
                        (Sequence [ Decimal 1m
                                    Decimal 2m
                                  ]))
                }

                test "multi type sequence with item leading space" {
                  Parser.run @"- 1
-   true
- 3"
                  |> succeed
                      (Expect.equal
                        "multi-type sequence"
                        (Sequence [ Decimal 1m
                                    Boolean true
                                    Decimal 3m
                                  ]))
                }

                test "number sequence with initial indentation" {
                  Parser.run @"   - 1
   - 2"
                  |> succeed
                      (Expect.equal
                        "number list must be parsed properly"
                        (Sequence [ Decimal 1m
                                    Decimal 2m
                                  ]))
                }

                test "sequence of compact sequence" {
                  Parser.run @"- - 'one' # Compact
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
                  Parser.run @"- ::vector
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