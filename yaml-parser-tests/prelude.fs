module Prelude

open YamlParser.Types

open FParsec
open Expecto
open Expecto.Flip

let succeed f = function
                | Success(result, _, _) -> f result
                | Failure(error, _, _)  -> Tests.failtest error

let fail f = function
             | Success(result, _, _) -> result
                                        |> sprintf "Should have failed but instead returned: %A"
                                        |> Tests.failtest
             | Failure(error, _, _)  -> f error

let testParser p = runParserOnString p { indent = 0L; context = BlockOut } ""