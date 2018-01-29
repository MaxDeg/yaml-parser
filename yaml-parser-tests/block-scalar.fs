module YamlParser.BlockStyle.Scalar.Tests

open YamlParser.Types
open YamlParser

open Expecto
open Expecto.Flip

open Prelude

let parser = Parser.bareDocument


[<Tests>]
let tests = testList "block-scalar"
              [ test "literal scalar with auto-detect indent" {
                  testParser parser "|\r\n literal\r\n \ttext\r\n"
                  |> succeed 
                      (Expect.equal
                        "literal text"
                        (String "literal\n\ttext\n"))
                }

                test "literal scalar with indent of 1 and strip" {
                  testParser parser "|1- # Both indicatorsr\r\n strip"
                  |> succeed 
                      (Expect.equal
                        "literal text"
                        (String "strip"))
                }

                test "literal scalar with keep" {
                  testParser parser "|+ # Chomping indicator\r\nkeep\r\n"
                  |> succeed 
                      (Expect.equal
                        "literal text"
                        (String "keep\n"))
                }

                test "literal scalar with indent of 5" {
                  testParser parser "|5 # Indentation indicator\r\n     folded"
                  |> succeed 
                      (Expect.equal
                        "literal text"
                        (String "folded"))
                }

                test "literal scalar with indent of 4 instead of 4" {
                  testParser parser "|5 # Indentation indicator\r\n    folded"
                  |> fail ignore
                }
                
                
                test "literal scalar with empty header" {
                  testParser parser "| # Empty header\r\n literal"
                  |> succeed 
                      (Expect.equal
                        "literal text"
                        (String "literal"))
                }
                
                test "literal scalars with multiple headers" {
                  testParser parser "- | # Empty header\r\n literal\r\n- |1 # Indentation indicator\r\n  folded\r\n- |+ # Chomping indicator\r\n keep\r\n\r\n- |1- # Both indicators\r\n  strip"
                  |> succeed 
                      (Expect.equal
                        "literal text"
                        (Sequence [ String "literal\n"
                                    String " folded\n"
                                    String "keep\n\n"
                                    String " strip"
                                  ]))
                }
              ]