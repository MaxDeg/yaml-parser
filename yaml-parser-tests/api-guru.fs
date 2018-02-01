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
  { swaggerYamlUrl: string
    swaggerUrl: string
  }

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
        printfn "Successfully parsed %s in %ims" url timer.ElapsedMilliseconds
    | Error error -> 
        Tests.failtest <| sprintf "Error parsing %s: %s" url error

  Request.createUrl Get url
  |> Request.responseAsString
  |> Hopac.run
  |> parse

let createList () =
  Request.createUrl Get "https://api.apis.guru/v2/list.json"
  |> Request.responseAsString
  |> Hopac.run
  |> JsonConvert.DeserializeObject<Dictionary<string, ApiGuruDefinition>>
  |> Seq.collect (fun d -> 
        let yamlUrl = d.Value.versions.[d.Value.preferred].swaggerYamlUrl
        let jsonUrl = d.Value.versions.[d.Value.preferred].swaggerUrl

        [ test (sprintf "try parse yaml api %s" yamlUrl) {
            downloadAndParse yamlUrl
          }
          test (sprintf "try parse yaml api %s" jsonUrl) {
            downloadAndParse jsonUrl
          }
        ])
  |> Seq.toList

[<Tests>]
let tests = 
  testList "api guru tests" (createList ())

