module YamlParser.FlowStyle.Tests

open YamlParser.Types
open YamlParser.FlowStyle

open FParsec
open Expecto
open Expecto.Flip


[<Tests>]
let tests =
  testList "flow-tyle"
    [ testCase "scalar-plain" <| fun _ ->
        let plainParser = Scalars.parser BlockIn 0L

        let parsePlain text expect = 
          run plainParser text
          |> function 
          | Success(result, _, _) -> Expect.equal "proper plain scalar" expect result
          | Failure(error, _, _)  -> Tests.failtest error
          
        let parsePlainText text = parsePlain text (String text)

        parsePlainText "::vector"
        parsePlainText @"Up, up, and away!"
        parsePlain @"-123" (Decimal -123.0m)
        parsePlainText @"http://example.com/foo#bar"
    ]