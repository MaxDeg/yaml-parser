#r "paket: nuget Fake.Core.Target //"
#r "paket: nuget Fake.Core.Trace //"
#r "paket: nuget Fake.Core.Environment //"
#r "paket: nuget Fake.Core.Process //"
#r "paket: nuget Fake.DotNet.Cli //"

#load ".fake/build.fsx/intellisense.fsx"

open System

open Fake.Core
open Fake.Core.TargetOperators

open Fake.IO
open Fake.IO.Globbing.Operators

open Fake.DotNet

// -----------------------------------------------------------------------------
// -- Variables
// -----------------------------------------------------------------------------
let buildDir  = "./build/"
let appReferences = !! "./**/*.fsproj"

// -----------------------------------------------------------------------------
// -- Targets
// -----------------------------------------------------------------------------
Target.create "Clean" <| fun _ ->
    Shell.cleanDir buildDir

Target.create "Restore" <| fun _ ->
    appReferences
    |> Seq.iter (fun p ->
        DotNet.restore (fun args -> { args with NoCache = true }) p
    )

Target.create "Build" <| fun _ ->
    appReferences
    |> Seq.iter (fun p ->
        DotNet.build (fun args -> 
            { args with Common =
                { args.Common with CustomParams = Some "--no-restore" }
            }) p
    )

Target.create "Tests" <| fun _ ->
    appReferences
    |> Seq.filter (fun p -> p.Contains "tests")
    |> Seq.iter (fun p -> 
        DotNet.test (fun args -> { args with NoRestore = true }) p |> ignore)

// -----------------------------------------------------------------------------
// -- Dependencies
// -----------------------------------------------------------------------------
"Clean"
  ==> "Restore"
  ==> "Build"
  ==> "Tests"

// -----------------------------------------------------------------------------
// -- Start build
// -----------------------------------------------------------------------------
Target.runOrDefault "Tests"
