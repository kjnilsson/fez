#!/usr/bin/env fsharpi
#r "./lib/FSharp.Compiler.Service.dll"
open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

let testModule = """module Test
   let square x = x * x
"""

let file = __SOURCE_DIRECTORY__ +  "/Test.fs"
File.WriteAllText(file, testModule)
let checker = FSharpChecker.Create(keepAssemblyContents=true)
let options =
  checker.GetProjectOptionsFromCommandLineArgs("Test", [|"-o:Test.dll";"-a";file|])
let checkProjectResults =
  checker.ParseAndCheckProject(options) |> Async.RunSynchronously
