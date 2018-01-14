module Prelude

open YamlParser.Types

open FParsec
open Expecto

let succeed f = function
                | Success(result, _, _) -> f result
                | Failure(error, _, _)  -> Tests.failtest error

let test p = runParserOnString p { indent = 0L; context = BlockOut } ""