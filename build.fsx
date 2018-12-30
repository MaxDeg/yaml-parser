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
Target.create "Clean" <| fun _ -> Shell.cleanDir buildDir

Target.create "Restore" <| fun _ ->
  let configureRestore (args : DotNet.RestoreOptions) =
    { args with NoCache = true }

  appReferences
  |> Seq.iter (DotNet.restore configureRestore)

Target.create "Build" <| fun _ ->
  let configureBuild (args : DotNet.BuildOptions) =
    { args with
        Common = { args.Common with
                    CustomParams = Some "--no-restore" } }

  appReferences
  |> Seq.iter (DotNet.build configureBuild)

Target.create "Tests" <| fun _ ->
  let configureTest (args : DotNet.TestOptions) =
    { args with NoRestore = true }

  appReferences
  |> Seq.filter (fun p -> p.Contains "tests")
  |> Seq.iter (fun p -> DotNet.test configureTest p |> ignore)

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
