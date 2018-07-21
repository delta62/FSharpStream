module JsonStream.Tests.Main

open Expecto
open Hopac
open Logary.Configuration
open Logary.Targets
open Logary.Adapters.Facade

[<EntryPoint>]
let main argv =
  let logary =
    Config.create "JsonStream.Tests" "localhost"
    |> Config.targets [ LiterateConsole.create LiterateConsole.empty "console" ]
    |> Config.processing (Events.events |> Events.sink [ "console"; ])
    |> Config.build
    |> run
  LogaryFacadeAdapter.initialise<Expecto.Logging.Logger> logary

  runTestsInAssembly defaultConfig argv
