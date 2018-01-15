module YamlParser.FlowStyle.Collection.Mapping.Tests

open YamlParser.Types
open YamlParser.FlowStyle

open Expecto
open Expecto.Flip

open Prelude

let parser = Collections.parser


[<Tests>]
let tests = testList "flow-mapping"
              [ test "simple mapping" {
                  testParser parser @"{
'number': 1,
bool: true,
? 'readable': value,
? 42: universal answer
}"
                  |> succeed 
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping <| Map.ofList [ String "number", Decimal 1.m
                                                 String "bool", Boolean true
                                                 String "readable", String "value"
                                                 Decimal 42.m, String "universal answer"
                                               ]))
                }


                test "mapping with adjacent value" {
                  testParser parser @"{
'adjacent':value,
'readable':'value'
}"
                  |> succeed 
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping <| Map.ofList [ String "adjacent", String "value"
                                                 String "readable", String "value"
                                               ]))
                }


                test "mapping with empty keys" {
                  testParser parser @"{
? 
}"
                  |> succeed 
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping <| Map.ofList [ Empty, Empty
                                               ]))
                  
                  testParser parser @"{
? : no key
}"
                  |> succeed 
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping <| Map.ofList [ Empty, String "no key"
                                               ]))

                  testParser parser @"{
: 'omitted key'
}"
                  |> succeed 
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping <| Map.ofList [ Empty, String "omitted key"
                                               ]))
                }


                test "mapping with empty values" {
                  testParser parser @"{
'omitted value':,
http://foo.com,
? 42:,
? 43: ,
'empty':
}"
                  |> succeed 
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping <| Map.ofList [ String "empty", Empty
                                                 String "http://foo.com", Empty
                                                 String "omitted value", Empty
                                                 Decimal 42.m, Empty
                                                 Decimal 43.m, Empty
                                               ]))
                }


                test "mapping with final ','" {
                  testParser parser @"{
unquoted : 'separate',
http://foo.com,
'omitted value':,
: omitted key,
}"
                  |> succeed
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping <| Map.ofList [ String "http://foo.com", Empty
                                                 String "omitted value", Empty
                                                 String "unquoted ", String "separate"
                                                 Empty, String "omitted key"
                                               ]))
                }


                test "no adjacent value with plain key" {
                  testParser parser @"{
plain text:should fail
}"
                  |> fail (fun f -> ())
                }

                test "inline mapping" {
                  testParser parser "{unquoted : 'separate', http://foo.com, 42: , : omitted key,}"
                  |> succeed
                      (Expect.equal
                        "correct mapping read"
                        (Mapping <| Map.ofList [ String "http://foo.com", Empty
                                                 String "unquoted ", String "separate"
                                                 Decimal 42m, Empty
                                                 Empty, String "omitted key"
                                               ]))
                }
              ]
