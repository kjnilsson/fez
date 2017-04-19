#!/usr/bin/env fsharpi
#r "./lib/FSharp.Compiler.Service.dll"
#load "./flatc/cerl.fs"
#load "./flatc/Program.fs"
open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

let testModule = """module Test
   let square x = x * x
"""

let testModule2 = """module Test
   let positive =
       function
       | 0 -> 1
       | n -> n
"""

let check testModule =
    let file = __SOURCE_DIRECTORY__ +  "/Test.fs"
    File.WriteAllText(file, testModule)
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let options =
      checker.GetProjectOptionsFromCommandLineArgs("Test", [|"-o:Test.dll";"-a";file|])
    let res =
      checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    assert(Array.empty = res.Errors)
    res

let res = check testModule
let decs = res.AssemblyContents.ImplementationFiles.Head.Declarations

let dec = decs.[0]
let Entity(ent, decls) = dec

