module Fez.Erlc

open System
open System.IO
open System.Diagnostics

/// Writes Erlang Core file to disk
let writeErlangCoreFile dir name text =
    let path = FileInfo(Path.Combine(dir, name + ".core")).FullName
    File.WriteAllText(path, text)
    path

/// Calls erlc with all generated files
let call outputPath files =
    let output =
        match outputPath with
        | Some outputPath -> outputPath
        | _ -> failwithf "Output path was not set."

    if Seq.isEmpty files then () else
    let out = new System.Collections.Generic.List<_>()
    let errors = new System.Collections.Generic.List<_>()
    use proc = new Process()
    proc.StartInfo.UseShellExecute <- false
    proc.StartInfo.FileName <- "erlc"
    proc.StartInfo.Arguments <- 
        let mutable args = sprintf "-v -o \"%s\"" output 
        for file in files do
            args <- args + sprintf " \"%s\"" file
        args
    proc.StartInfo.WorkingDirectory <- output
    proc.StartInfo.RedirectStandardOutput <- true
    proc.StartInfo.RedirectStandardError <- true
    
    proc.ErrorDataReceived.Add(fun d -> if not (isNull d.Data) then errors.Add d.Data)
    proc.OutputDataReceived.Add(fun d -> if not (isNull d.Data) then out.Add d.Data)
    proc.Start() |> ignore
    proc.BeginErrorReadLine()
    proc.BeginOutputReadLine()
    proc.WaitForExit()
    if proc.ExitCode <> 0 then
        let lines = String.Concat(errors |> Seq.map (fun e -> Environment.NewLine + e))
        failwithf "erlc failed: %s" lines
