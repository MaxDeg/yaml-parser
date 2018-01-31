module ApiGuru.Tests

open System.Diagnostics
open System.Collections.Generic

open Expecto
open Newtonsoft.Json
open HttpFs.Client
open Hopac

open YamlParser

[<CLIMutable>]
type ApiDefinition = 
  { swaggerYamlUrl: string }

[<CLIMutable>]
type ApiGuruDefinition =
  { preferred:  string
    versions:   Dictionary<string, ApiDefinition> }

let downloadAndParse url = 
  let parse def = 
    let timer = Stopwatch.StartNew()
    let result = Parser.run def
    timer.Stop()
    
    match result with
    | Ok _ -> 
        printfn "Successfully parsed in %ims" timer.ElapsedMilliseconds
    | Error error -> 
        Tests.failtest <| sprintf "Error: %A" error
        //timer.ElapsedMilliseconds

  Request.createUrl Get url
  |> Request.responseAsString
  |> Hopac.run
  |> parse

[<Tests>]
let tests = 
  testList "api guru tests"
    [ test "try parse all api on api guru" {         
        Request.createUrl Get "https://api.apis.guru/v2/list.json"
        |> Request.responseAsString
        |> Hopac.run
        |> JsonConvert.DeserializeObject<Dictionary<string, ApiGuruDefinition>>
        |> Seq.map (fun d -> d.Value.versions.[d.Value.preferred].swaggerYamlUrl)
        |> Seq.iter downloadAndParse
      }
    ]

