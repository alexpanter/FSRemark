module Library
open System
open System.Text.RegularExpressions


// Marks:
// mark the beginning of a line
let mark_section         = "#"
let mark_question        = "*"
let mark_feedback_good   = "+"
let mark_feedback_bad    = "-"
let mark_feedback_unsure = "?"


// Position:
// Track the file position (line number) when a syntax error occurs
type Position = int


// Error handling:
// Print custom error message, then terminate program
type MyException =
    | MyIOException of string
    | MyFormatException of string
    | MyParseException of string

let errorHandler (err: MyException) =
    let mutable str = ""
    match err with
        | MyIOException s     -> printfn "IO error: %s" s
        | MyFormatException s -> printfn "Format error: %s" s
        | MyParseException s  -> printfn "Parse error: %s" s
    Environment.Exit 1


// Extended pattern:
// Allow recursive pattern matching on strings
let (|StartsWith|_|) (s: string) (p: string) =
    if p.StartsWith(s) then Some(p.Substring(s.Length))
    else None


// IO functions:
// Convenient wrappers for file-IO
let openFile (path: string) =
    if not <| IO.File.Exists path then
        let str = "The file does not exist"
        errorHandler (MyIOException(str))
        null
    else if IO.Path.GetExtension (path) <> ".mrk" then
        let str = "The file must have the .mrk extension"
        errorHandler (MyIOException(str))
        null
    else
        new System.IO.StreamReader(path)

let readLine(f: IO.StreamReader) =
    if not <| f.EndOfStream then
        Some(f.ReadLine())
    else None


// Formatted types:
// Store each line from the file in a formatted buffer of records
type ContentQuestion = {contents: string}
type ContentFeedback = {mark: string; feedback: string}
type ContentSection = {depth: int; title: string; points: int}

type FormattedContent =
    | FormattedSection  of ContentSection
    | FormattedQuestion of ContentQuestion
    | FormattedFeedback of ContentFeedback
    | FormattedEmpty
    | FormattedError of MyException

type FormattedFile =
    | FormattedLines of FormattedContent list

type FormatLineType =
    | Section
    | Question
    | Feedback
    | Empty



// Formatting functions
// Format a file into a structured list of line type records
let formatHandleError (str: string) =
    errorHandler (MyFormatException str)
    FormattedEmpty

let rec formatFeedback (line: string) =
    let pattern = "^[\s]*([\+\?-])[ ]+([\w\W\d\s]+)$"
    if Regex.IsMatch(line, pattern) then
        let res = Regex.Split(line, pattern)
        match res.[1] with
        | "+" -> FormattedFeedback ({mark = mark_feedback_good;
                                     feedback = res.[2]})
        | "-" -> FormattedFeedback ({mark = mark_feedback_bad;
                                     feedback = res.[2]})
        | "?" -> FormattedFeedback ({mark = mark_feedback_unsure;
                                     feedback = res.[2]})
        | _   -> "Internal error: Unexpected symbol in format feedback"
                 |> formatHandleError
    else
        "Section feedback is malformed" |> formatHandleError

let rec formatQuestion (line: string) =
    let pattern = "^[\s]*[\*][ ]+([\w\W\d\s]+)$"
    if Regex.IsMatch(line, pattern) then
        let res = Regex.Split(line, pattern)
        FormattedQuestion({contents = res.[1]})
    else
        "Section question is malformed" |> formatHandleError

let formatSection (line: string) =
    let pattern = "^([#]+)[ ]+([\w\d ]+): /([\d]+[\d]*)$"
    if Regex.IsMatch(line, pattern) then
        let res = Regex.Split(line, pattern)
        FormattedSection({depth = int (res.[1].Length); title = res.[2]; points = int (res.[3])})
    else
        "Section header is malformed" |> formatHandleError

let rec formatGetType = function
    | ""                  -> FormatLineType.Empty
    | StartsWith " " rest -> formatGetType rest
    | StartsWith "#" rest -> FormatLineType.Section
    | StartsWith "+" rest -> FormatLineType.Feedback
    | StartsWith "-" rest -> FormatLineType.Feedback
    | StartsWith "?" rest -> FormatLineType.Feedback
    | StartsWith "*" rest -> FormatLineType.Question
    | _ -> "Invalid line" |> formatHandleError |> ignore; FormatLineType.Empty

let formatFile (path: string) =
    let file = openFile(path)
    let rec inner() =
        match readLine(file) with
        | Some s ->
            match formatGetType s with
            | FormatLineType.Empty    -> FormattedContent.FormattedEmpty :: inner()
            | FormatLineType.Section  -> formatSection s :: inner()
            | FormatLineType.Question -> formatQuestion s :: inner()
            | FormatLineType.Feedback -> formatFeedback s :: inner()
        | None -> []
    inner()


// Parse types:
// Build a recursive tree type from a formatted records buffer
type ParsedQuestion =
    | Question of ContentQuestion * ContentFeedback list

type ParsedSection =
    | Section of ParsedQuestion list

type ParsedFile =
    | File of ParsedSection list


// Parse functions:
// Functions to parse a formatted file into a parsed file
let rec parseFile' (file: FormattedContent list)
                   (last: FormattedContent) =
    match file with
    | [] -> []
    | FormattedSection s :: xs  ->
        let sect = parseSection xs (FormattedSection s)
        Section(sect) :: parseFile' xs (FormattedSection s)
    | FormattedQuestion q :: xs -> []
    | FormattedFeedback f :: xs -> []
    | FormattedError e :: xs    -> errorHandler e; []
    | FormattedEmpty :: xs      -> []

and parseSection file last =
    match file with
    | FormattedSection s :: xs -> []
    | FormattedFeedback f :: xs ->
        let str = "Feedback given without a question"
        errorHandler (MyParseException(str))
        []
    | _ -> []



// Wrapper function for parsing a formatted file
let parseFile file = parseFile' file FormattedEmpty


