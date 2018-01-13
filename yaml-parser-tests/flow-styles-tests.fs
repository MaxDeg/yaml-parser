module YamlParser.FlowStyle.Tests

open YamlParser.Types
open YamlParser.FlowStyle

open FParsec
open Expecto
open Expecto.Flip

let succeed f = function
                | Success(result, _, _) -> f result
                | Failure(error, _, _)  -> Tests.failtest error

[<Tests>]
let scalar =
  let parser = Scalars.parser BlockIn 0L
  testList "flow-style-scalar"
    [ testCase "plain" <| fun _ ->
        let parsePlain text expect = 
          run parser text
          |> succeed (Expect.equal "proper plain scalar" expect)
          
        let parsePlainText text = parsePlain text (String text)

        parsePlainText "::vector"
        parsePlainText @"Up, up, and away!"
        parsePlain @"-123" (Decimal -123.0m)
        parsePlainText @"http://example.com/foo#bar"

      testCase "flow-folded" <| fun _ ->
        run parser "'trimmed\r\n  \r\n \r\n\r\nas\r\nspace'"
        |> succeed (Expect.equal
                      "flow folded properly parsed"
                      (String "trimmed\n\n\nas space"))

        run parser @"'trimmed
  
 

as
space'"
        |> succeed (Expect.equal
                      "flow folded properly parsed"
                      (String "trimmed\n\n\nas space"))
        
        run parser @"'
  foo 
 
     bar

  baz
'"
        |> succeed (Expect.equal
                      "flow folded properly parsed"
                      (String " foo\nbar\nbaz "))

      testCase "single quoted with single quote inside" <| fun _ ->
        run parser "'here''s to \"quotes\"'"
        |> succeed (Expect.equal
                      "with single quote inside properly parsed"
                      (String "here's to \"quotes\""))

      testCase "single quoted multiline" <| fun _ ->
        run parser "' 1st non-empty\r\n\r\n 2nd non-empty\r\n\t3rd non-empty '"
        |> succeed (Expect.equal
                      "multiline properly parsed"
                      (String " 1st non-empty\n2nd non-empty 3rd non-empty "))
    ]

[<Tests>]
let collection =
  let parser = Collections.parser BlockIn 0L

  testList "flow-style-sequence"
    [ testCase "multi-kind items" <| fun _ ->
        run parser @"[
""double quoted"", 'single quoted',
   [ nested ]
]"
        |> succeed (Expect.equal 
                      "Correct sequence read"
                      (Sequence [ (String "double quoted")
                                  (String "single quoted")
                                  (Sequence [ String "nested " ])
                                ]))

      testCase "with comments inside" <| fun _ ->
        run parser @"[ # inline comment
# on line comment
1,
# separation comment
2    ]"
        |> succeed (Expect.equal
                      "comments in list properly parsed"
                      (Sequence [ (Decimal 1.0m)
                                  (Decimal 2.0m)
                                ]))
                                 
      testCase "playing with ','" <| fun _ ->
        run parser "[ 'one', two, ]"
        |> succeed (Expect.equal
                      "final ',' is properly parsed"
                      (Sequence [ (String "one")
                                  (String "two")
                                ]))
        
        run parser "['three'   ,'four']"
        |> succeed (Expect.equal
                      "spaces before ',' properly parsed"
                      (Sequence [ (String "three")
                                  (String "four")
                                ]))
                                
        run parser @"[ 1, 2,
3
, 4
,
5]"
        |> succeed (Expect.equal
                      "multiline ',' parsed properly"
                      (Sequence [ (Decimal 1.0m)
                                  (Decimal 2.0m)
                                  (Decimal 3.0m)
                                  (Decimal 4.0m)
                                  (Decimal 5.0m)
                                ])) 
    ]