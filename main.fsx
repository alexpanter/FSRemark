#load "library.fs"

//type flags = {}

[<EntryPoint>]
let main (args: string[]) =
    if args.Length = 1 then
        args.[0]
        |> Library.formatFile
        |> Library.parseFile
        |> Library.htmlParser
        0
    else
        printfn "Please provide a file name for reference"
        1
