#time

#load "../.paket/load/netstandard1.6/main.group.fsx"
#load "prelude.fs"
#load "types.fs"
#load "primitives.fs"
#load "flow-styles.fs"
#load "block-styles.fs"
#load "parser.fs"

open Prelude
open YamlParser

let content f = 
  System.IO.File.ReadAllText(f)


content @"C:\Users\mdg\AppData\Local\Temp\swagger.yaml"
|> Parser.run
|> function Ok _ -> ignore () | Error e -> printfn "%s" e

content @"C:\Users\mdg\Downloads\swagger.json"
|> Parser.run
|> function Ok _ -> ignore () | Error e -> printfn "%s" e


Parser.run "' 1st non-empty\r\n\r\n 2nd non-empty\r\n\t3rd non-empty '"


// unit test
Parser.run ">\r\n folded\r\n text\r\n\r\n"

Parser.run ">1+\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment"

Parser.run ">+1\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment"


Parser.run ">-\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment"

Parser.run ">\n\n folded\n line\n\n next\n line\n   * bullet\n\n   * list\n   * lines\n\n last\n line\n\n# Comment"


// unit test

Parser.run @"
      description: |
        Manually emails all the billing documents that are generated from a specified bill run to your customers. 


        Bill runs can generate invoices and credit memos based on your [invoice and credit memo generation rule](https://knowledgecenter.zuora.com/CB_Billing/Advanced_AR_Settlement/Credit_and_Debit_Memos/Rules_for_Generating_Invoices_and_Credit_Memos). Credit memos are only available if you have the Advanced AR Settlement feature enabled.


        Using this API operation, the billing documents are sent to the email addresses specified in the **To Email** field of the email templates. The email template used for each billing document is set in the **Delivery Options** panel of the **Edit notification** dialog from the Zuora UI. See [Edit Email Templates](https://knowledgecenter.zuora.com/CF_Users_and_Administrators/Notifications/Create_Email_Templates) for more information about how to edit the **To Email** field in the email template.





        ## Notes
          - Even though no field is required in the Request body, you still need to specify `{}` in the request. Otherwise, an error will be returned.


          - You can only email posted billing documents.
          
          
          - You must activate the following notifications before emailing invoices and credit memos:
            - **Manual Email For Invoice | Manual Email For Invoice** 
            - **Email Credit Memo | Manually email Credit Memo**
         
          
          - To include the invoice PDF in the email, select the **Include Invoice PDF** check box in the **Edit notification** dialog from the Zuora UI. To include the credit memo PDF in the email, select the **Include Credit Memo PDF** check box in the **Edit notification** dialog from the Zuora UI. See [Create and Edit Notifications](https://knowledgecenter.zuora.com/CF_Users_and_Administrators/Notifications/C_Create_Notifications#section_2) for more information.



          - Zuora sends the email messages based on the email template you set. You can set the email template to use in the **Delivery Options** panel of the **Edit notification** dialog from the Zuora UI. By default, the following templates are used for billing documents:
            - Invoices: **Invoice Posted Default Email Template**
            - Credit memos: **Manual Email for Credit Memo Default Template**  

            See [Create and Edit Email Templates](https://knowledgecenter.zuora.com/CF_Users_and_Administrators/Notifications/Create_Email_Templates) for more information.
          
"



showEscaped @">

 folded
 line

 next
 line
   * bullet

   * list
   * lines

 last
 line

# Comment"


Parser.run @"- | # Empty header
 literal
  folded
 keep
  strip"

Parser.run "| # Empty header\r\n literal"

Parser.run "|5 # Indentation indicator\r\n     folded"

Parser.run "|+ # Chomping indicator\r\nkeep\r\n"


Parser.run "|1 # Both indicatorsr\r\n strip"

Parser.run "|\r\n literal\r\n \ttext\r\n"

Parser.run "\"they’re \\\" planning to travel.\""

Parser.run "\"one tab \\t later\""
Parser.run "#only a comment"


Parser.run @"""implicit block key"" : [
  ""implicit flow key"" : value,
 ]"

Parser.run "\"\r\n  foo \r\n\r\n  \t bar\r\n\r\n  baz\r\n\""

Parser.run "\"folded \r\nto a space,\t\r\n \r\nto a line feed, or \t\\\r\n \\ \tnon-content\""


Parser.run @""" 1st non-empty

 2nd non-empty 
  3rd non-empty """

Parser.run @"plain key: in-line value
: # Both empty
""quoted key"":
- entry
"

Parser.run  @"- sun: yellow
- ? earth: blue
  : moon: white"

Parser.run "{unquoted : 'separate'}"

Parser.run "{unquoted : 'separate', http://foo.com, 42: , : omitted key,}"

Parser.run @"{
omitted value:,
http://foo.com,
? 42:,
? 43: ,
'empty':
}"

Parser.run @"{
'adjacent':value,
'readable': value,
'empty':
}"

Parser.run @"{
? explicit: entry,
implicit: entry,
?
}"

Parser.run @"{
unquoted : 'separate',
http://foo.com,
omitted value:,
: omitted key,
}"

Parser.run @"- 1
- 2"

Parser.run @"- 1
-   true
- 3"

Parser.run @"   - 1
   - 2"

Parser.run @"- - 'one' # Compact
  - 'two' # sequence"


Parser.run @"- one: two # Compact mapping"


Parser.run @"? - 1
  - 2
: 3"

Parser.run @"? - 1
  - 2 : 3
: 4"

Parser.run @"- ::vector
- Up, up, and away!
- -123
- http://example.com/foo#bar"

Parser.run @"- [ ::vector,
  "": - ()"",
  ""Up, up and away!"",
  -123,
  http://example.com/foo#bar ]"

Parser.run @"[
# my comment
""double
 quoted"", 'single
           quoted',
   [ nested ]
]"

Parser.run @"- [ one, two, ]
- [three ,four]"

Parser.run @"[ 1, 2,
3
, 4
,
5]"


Parser.run @"[ 1,  
2
]"


Parser.run @"'quoted key':
- entry"


Parser.run @"block mapping:
 key: value"

Parser.run @"? 'block key'
: - one # Explicit compact
  - two # block value
"

Parser.run  @"[ # inline comment
# on line comment
1,
# separation comment
2    ]"

Parser.run @"[
? 'foo
 bar' : baz
]"