module YamlParser.FlowStyle.Collection.Mapping.Tests

open YamlParser.Types
open YamlParser

open Expecto
open Expecto.Flip

open Prelude

[<Tests>]
let tests = testList "flow-mapping"
              [ test "simple mapping" {
                  Parser.run @"{
'number': 1,
bool: true,
? 'readable': value,
? 42: universal answer
}"
                  |> succeed 
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping [ String "number", Decimal 1m
                                   String "bool", Boolean true
                                   String "readable", String "value"
                                   Decimal 42m, String "universal answer"
                                ]))
                }


                test "mapping with adjacent value" {
                  Parser.run @"{
'adjacent':value,
'readable':'value'
}"
                  |> succeed 
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping [ String "adjacent", String "value"
                                   String "readable", String "value"
                                ]))
                }


                test "mapping with empty keys" {
                  Parser.run @"{
? 
}"
                  |> succeed 
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping [ Empty, Empty
                                               ]))
                  
                  Parser.run @"{
? : no key
}"
                  |> succeed 
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping [ Empty, String "no key"
                                               ]))

                  Parser.run @"{
: 'omitted key'
}"
                  |> succeed 
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping [ Empty, String "omitted key"
                                               ]))
                }


                test "mapping with empty values" {
                  Parser.run @"{
'omitted value':,
http://foo.com,
? 42:,
? 43: ,
'empty':
}"
                  |> succeed 
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping [ String "empty", Empty
                                   String "http://foo.com", Empty
                                   String "omitted value", Empty
                                   Decimal 42m, Empty
                                   Decimal 43m, Empty
                                ]))
                }


                test "mapping with final ','" {
                  Parser.run @"{
unquoted : 'separate',
http://foo.com,
'omitted value':,
: omitted key,
}"
                  |> succeed
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping [ String "http://foo.com", Empty
                                   String "omitted value", Empty
                                   String "unquoted", String "separate"
                                   Empty, String "omitted key"
                                ]))
                }


                test "no adjacent value with plain key" {
                  Parser.run @"{
plain text:should fail
}"
                  |> succeed
                      (Expect.equal
                        "inline mapping with empty value"
                        (Mapping
                          [ String "plain text:should fail", Empty ]))
                }

                test "inline mapping" {
                  Parser.run "{unquoted : 'separate', http://foo.com, 42: , : omitted key,}"
                  |> succeed
                      (Expect.equal
                        "correct mapping read"
                        (Mapping [ String "http://foo.com", Empty
                                   String "unquoted", String "separate"
                                   Decimal 42m, Empty
                                   Empty, String "omitted key"
                                ]))
                }
              ]
