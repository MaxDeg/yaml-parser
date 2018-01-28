module YamlParser.FlowStyle.Collection.Mapping.Tests

open YamlParser.Types
open YamlParser

open Expecto
open Expecto.Flip

open Prelude

let parser = Parser.bareDocument


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
                        (Mapping [ String "number", String "1"
                                   String "bool", Boolean true
                                   String "readable", String "value"
                                   String "42", String "universal answer"
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
                        (Mapping [ String "adjacent", String "value"
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
                        (Mapping [ Empty, Empty
                                               ]))
                  
                  testParser parser @"{
? : no key
}"
                  |> succeed 
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping [ Empty, String "no key"
                                               ]))

                  testParser parser @"{
: 'omitted key'
}"
                  |> succeed 
                      (Expect.equal 
                        "Correct mapping read"
                        (Mapping [ Empty, String "omitted key"
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
                        (Mapping [ String "empty", Empty
                                   String "http://foo.com", Empty
                                   String "omitted value", Empty
                                   String "42", Empty
                                   String "43", Empty
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
                        (Mapping [ String "http://foo.com", Empty
                                   String "omitted value", Empty
                                   String "unquoted", String "separate"
                                   Empty, String "omitted key"
                                ]))
                }


                test "no adjacent value with plain key" {
                  testParser parser @"{
plain text:should fail
}"
                  |> succeed
                      (Expect.equal
                        "inline mapping with empty value"
                        (Mapping
                          [ String "plain text:should fail", Empty ]))
                }

                test "inline mapping" {
                  testParser parser "{unquoted : 'separate', http://foo.com, 42: , : omitted key,}"
                  |> succeed
                      (Expect.equal
                        "correct mapping read"
                        (Mapping [ String "http://foo.com", Empty
                                   String "unquoted", String "separate"
                                   String "42", Empty
                                   Empty, String "omitted key"
                                ]))
                }
              ]
