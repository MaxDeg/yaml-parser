module YamlParser.BlockStyle.Scalar.Tests

open YamlParser.Types
open YamlParser

open Expecto
open Expecto.Flip

open Prelude

[<Tests>]
let tests = testList "block-scalar"
              [ test "literal scalar with auto-detect indent" {
                  Parser.run "|\r\n literal\r\n \ttext\r\n"
                  |> succeed 
                      (Expect.equal
                        "literal text"
                        (String "literal\n\ttext\n"))
                }

                test "literal scalar with indent of 1 and strip" {
                  Parser.run "|1- # Both indicatorsr\r\n strip"
                  |> succeed 
                      (Expect.equal
                        "literal text"
                        (String "strip"))
                }

                test "literal scalar with strip and indent of 1" {
                  Parser.run "|-1 # Both indicatorsr\r\n strip"
                  |> succeed 
                      (Expect.equal
                        "literal text"
                        (String "strip"))
                }

                test "literal scalar with keep" {
                  Parser.run "|+ # Chomping indicator\r\nkeep\r\n"
                  |> succeed 
                      (Expect.equal
                        "literal text"
                        (String "keep\n"))
                }

                test "literal scalar with indent of 5" {
                  Parser.run "|5 # Indentation indicator\r\n     folded"
                  |> succeed 
                      (Expect.equal
                        "literal text"
                        (String "folded"))
                }

                test "literal scalar with indent of 4 instead of 4" {
                  Parser.run "|5 # Indentation indicator\r\n    folded"
                  |> fail ignore
                }
                
                
                test "literal scalar with empty header" {
                  Parser.run "| # Empty header\r\n literal"
                  |> succeed 
                      (Expect.equal
                        "literal text"
                        (String "literal"))
                }
                
                test "literal scalars with multiple headers" {
                  Parser.run "- | # Empty header\r\n literal\r\n- |1 # Indentation indicator\r\n  folded\r\n- |+ # Chomping indicator\r\n keep\r\n\r\n- |1- # Both indicators\r\n  strip"
                  |> succeed 
                      (Expect.equal
                        "literal text"
                        (Sequence [ String "literal\n"
                                    String " folded\n"
                                    String "keep\n\n"
                                    String " strip"
                                  ]))
                }
                
                test "folded scalar with empty header" {
                  Parser.run ">\r\n  folded\r\n  text\r\n\r\n"
                  |> succeed 
                      (Expect.equal
                        "folded text"
                        (String "folded text\n"))
                }
                
                test "folded scalar with indentation 1 header" {
                  Parser.run ">1\r\n folded\r\n text\r\n\r\n"
                  |> succeed 
                      (Expect.equal
                        "folded text"
                        (String "folded text\n"))
                }
                
                test "folded scalar with indentation 2 header, wrong indentation" {
                  Parser.run ">2\r\n folded\r\n text\r\n\r\n"
                  |> fail ignore
                }
                                
                test "folded scalar with empty header and multi lines" {
                  Parser.run ">\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment"
                  |> succeed 
                      (Expect.equal
                        "folded text"
                        (String "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n"))
                }
                
                test "folded scalar with strip header and multi lines" {
                  Parser.run ">-\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment"
                  |> succeed 
                      (Expect.equal
                        "folded text"
                        (String "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line"))
                }
                
                test "folded scalar with keep header and multi lines" {
                  Parser.run ">+\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment"
                  |> succeed 
                      (Expect.equal
                        "folded text"
                        (String "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n\n"))
                }
                
                test "folded scalar with indent and keep header" {
                  Parser.run ">1+\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment"
                  |> succeed 
                      (Expect.equal
                        "folded text"
                        (String "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n\n"))
                }
                
                test "folded scalar with keep and indent header" {
                  Parser.run ">+1\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment"
                  |> succeed 
                      (Expect.equal
                        "folded text"
                        (String "\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n\n"))
                }
              ]