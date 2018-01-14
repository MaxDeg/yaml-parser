module YamlParser.BlockStyle.Collection.Sequence.Tests

open YamlParser.Types
open YamlParser.BlockStyle

open Expecto
open Expecto.Flip

open Prelude

let ``simple number sequence`` =
  testCase "simple number sequence" <| fun _ ->
    test parser @"- 1
- 2"
    |> succeed (Expect.equal
                  "number list must be parsed properly"
                  (Sequence [ Decimal 1.0m
                              Decimal 2.0m
                            ]))

let ``multi type sequence with item leading space`` =
  testCase "multi type sequence with item leading space" <| fun _ ->
    test parser @"- 1
-   true
- 3"
    |> succeed (Expect.equal
                  "multi-type sequence"
                  (Sequence [ Decimal 1.0m
                              Boolean true
                              Decimal 3.0m
                            ]))

let ``number sequence with initial indentation`` =
  testCase "number sequence with initial indentation" <| fun _ ->
    test parser @"   - 1
   - 2"
    |> succeed (Expect.equal
                  "number list must be parsed properly"
                  (Sequence [ Decimal 1.0m
                              Decimal 2.0m
                            ]))

let ``sequence of compact sequence`` =
  testCase "sequence of compact sequence" <| fun _ ->
    test parser @"- - 'one' # Compact
  - 'two' # sequence"
    |> succeed (Expect.equal
                  "number list must be parsed properly"
                  (Sequence [ Sequence [ String "one"
                                         String "two"
                                       ]
                            ]))

let ``sequence of scalar types`` =
  testCase "sequence of scalar types" <| fun _ ->
    test parser @"- ::vector
- Up, up, and away!
- -123
- http://example.com/foo#bar"
    |> succeed (Expect.equal
                  "number list must be parsed properly"
                  (Sequence [ String "::vector"
                              String "Up, up, and away!"
                              Decimal -123m
                              String "http://example.com/foo#bar"
                            ]))


[<Tests>]
let scalar =
  let parser = Collections.sequence
  testList "block-sequence" [ ``simple number sequence``
                              ``multi type sequence with item leading space``
                              ``number sequence with initial indentation``
                              ``sequence of compact sequence``
                              ``sequence of scalar types``
                            ]