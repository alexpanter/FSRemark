module FileIO

// system includes
open System

// custom includes
#load "types.fsx"
#load "error.fsx"
open Types
open Error


// IO functions:
// Convenient wrappers for file I/O
let openFile (path: string) (extension: string) =
    if not <| IO.File.Exists path then
        let str = "The file does not exist"
        errorHandler (MyIOException(str), 0)
        null
    else if IO.Path.GetExtension (path) <> extension then
        let str = "The file must have the "+extension+" extension"
        errorHandler (MyIOException(str), 0)
        null
    else
        new System.IO.StreamReader(path, Text.Encoding.UTF8)

let readLine(f: IO.StreamReader) =
    if not <| f.EndOfStream then
        Some(f.ReadLine())
    else None

let rec writeFile (path: string) =
    if IO.File.Exists(path) then
        printf "File '%s' already exists. Overwrite? (Y/N) " path
        match Console.ReadLine() with
            | "y" | "Y" -> ()
            | "n" | "N" | _ ->
                let str = "No output file was produced!"
                errorHandler(MyIOException(str), 0)
        IO.File.Delete(path)
    new IO.StreamWriter(path, true, Text.Encoding.UTF8)

// this function is not currently being used
let appendFile (source: IO.StreamReader) (destination: IO.StreamWriter) =
    while not <| source.EndOfStream do
        source.ReadLine() |> destination.WriteLine

