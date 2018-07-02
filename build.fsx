#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.DotNet.MSBuild
nuget Fake.DotNet.Paket
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators

let buildDir = "./build/"

Target.create "Clean" (fun _ ->
    Shell.cleanDir buildDir
)

Target.create "Restore" (fun _ ->
    Paket.restore id
)

Target.create "BuildApp" (fun _ ->
    !! "JsonStream/**/*.fsproj"
        |> MSBuild.runRelease id buildDir "Build"
        |> Trace.logItems "AppBuild-Output: "
)

"Clean"
  ==> "Restore"
  ==> "BuildApp"

Target.runOrDefault "BuildApp"
