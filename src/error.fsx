module Error

open System

#load "types.fsx"
open Types


// Print error message, then terminate program
let errorHandler (err: MyException, linenumber: int) =
    Console.ForegroundColor <- ConsoleColor.Red
    match err with
        | MyIOException s     ->
            printfn "IO error: %s" s
        | MyFormatException s ->
            printfn "Format error (line %i): %s" linenumber s
        | MyParseException s  ->
            printfn "Parse error (line %i): %s" linenumber s
        | MyHTMLException s ->
            printfn "Error parsing HTML: %s" s
    Console.ResetColor()
    Environment.Exit 1

// Print error message, but continue program
let warningHandler (err: MyException, linenumber: int) =
    Console.ForegroundColor <- ConsoleColor.Cyan
    match err with
        | MyIOException s     ->
            printfn "IO warning: %s" s
        | MyFormatException s ->
            printfn "Format warning (line %i): %s" linenumber s
        | MyParseException s  ->
            printfn "Parse warning (line %i): %s" linenumber s
        | MyHTMLException s ->
            printfn "HTML Warning: %s" s
    Console.ResetColor()
