module YamlParser.FlowStyle.Scalar.Tests

open YamlParser.Types
open YamlParser.FlowStyle

open Expecto
open Expecto.Flip

open Prelude

let parser = Scalars.parser


let ``parsing plain text`` =
  testCase "parsing plain text" <| fun _ ->
    let parsePlain text expect = 
      test parser text
      |> succeed (Expect.equal "proper plain scalar" expect)
      
    let parsePlainText text = parsePlain text (String text)

    parsePlainText "::vector"
    parsePlainText @"Up, up, and away!"
    parsePlain @"-123" (Decimal -123.0m)
    parsePlainText @"http://example.com/foo#bar"

let ``parse folded`` =
  testCase "flow-folded" <| fun _ ->
    test parser "'trimmed\r\n  \r\n \r\n\r\nas\r\nspace'"
    |> succeed (Expect.equal
                  "flow folded properly parsed"
                  (String "trimmed\n\n\nas space"))

    test parser @"'trimmed



as
space'"
    |> succeed (Expect.equal
                  "flow folded properly parsed"
                  (String "trimmed\n\n\nas space"))
    
    test parser @"'
  foo 
 
     bar

  baz
'"
    |> succeed (Expect.equal
                  "flow folded properly parsed"
                  (String " foo\nbar\nbaz "))

let ``single quoted with single quote inside`` =
  testCase "single quoted with single quote inside" <| fun _ ->
    test parser "'here''s to \"quotes\"'"
    |> succeed (Expect.equal
                  "with single quote inside properly parsed"
                    (String "here's to \"quotes\""))

let ``single quoted multiline`` =
  testCase "single quoted multiline" <| fun _ ->
    test parser "' 1st non-empty\r\n\r\n 2nd non-empty\r\n\t3rd non-empty '"
    |> succeed (Expect.equal
                  "multiline properly parsed"
                  (String " 1st non-empty\n2nd non-empty 3rd non-empty "))


[<Tests>]
let scalar =
  let parser = Collections.sequence
  testList "flow-scalar" [ ``parsing plain text``
                           ``parse folded``
                           ``single quoted with single quote inside``
                           ``single quoted multiline``
                         ]