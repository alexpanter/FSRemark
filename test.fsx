#load "library.fs"

//type flags = {}

[<EntryPoint>]
let main (args: string[]) =
    if args.Length = 1 then
        printfn "%A" <| Library.formatFile args.[0]
        0
    else
        printfn "Please provide a file name for reference"
        1
