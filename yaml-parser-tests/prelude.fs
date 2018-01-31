module Prelude

open Expecto

let succeed f = function
                | Ok result -> f result
                | Error error  -> Tests.failtest error

let fail f = function
             | Ok result -> result
                                        |> sprintf "Should have failed but instead returned: %A"
                                        |> Tests.failtest
             | Error error  -> f error
