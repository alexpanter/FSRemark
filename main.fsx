#load "library.fs"

open System.IO
open System.Collections.Generic

let produceHTML (path: string) =
    (Library.formatFile >> Library.parseFile >> Library.htmlParser) path

let performPassTests () =
    let files = Directory.EnumerateFiles (Path.Combine("tests", "pass"))
    let enum = files.GetEnumerator()
    while enum.MoveNext() do
        try
            enum.Current |> Library.formatFile |> Library.parseFile |> ignore
            printfn "SUCCESS: %s" enum.Current
        with
            | _ -> printfn "FAILED: %s" enum.Current


[<EntryPoint>]
let main (args: string[]) =
    if args.Length = 1 then
        produceHTML args.[0]
        0
    else
        printfn "Please provide a file name for reference"
        1
