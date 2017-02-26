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

let errorHandler (err: MyException, linenumber: int) =
    Console.ForegroundColor <- ConsoleColor.Red
    match err with
        | MyIOException s     ->
            printfn "IO error: %s" s
        | MyFormatException s ->
            printfn "Format error (line %i): %s" linenumber s
        | MyParseException s  ->
            printfn "Parse error (line %i): %s" linenumber s
    Console.ResetColor()
    Environment.Exit 1

let warningHandler (err: MyException, linenumber: int) =
    Console.ForegroundColor <- ConsoleColor.Cyan
    match err with
        | MyIOException s     ->
            printfn "IO warning: %s" s
        | MyFormatException s ->
            printfn "Format warning (line %i): %s" linenumber s
        | MyParseException s  ->
            printfn "Parse warning (line %i): %s" linenumber s
    Console.ResetColor()


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
        errorHandler (MyIOException(str), 0)
        null
    else if IO.Path.GetExtension (path) <> ".mrk" then
        let str = "The file must have the .mrk extension"
        errorHandler (MyIOException(str), 0)
        null
    else
        new System.IO.StreamReader(path)

let readLine(f: IO.StreamReader) =
    if not <| f.EndOfStream then
        Some(f.ReadLine())
    else None

let writeFile (path: string) =
    if IO.File.Exists(path) then
        printfn "overwriting file! Press key to continue "
        Console.ReadLine() |> ignore
    new IO.StreamWriter(path, false)


// Formatted types:
// Store each line from the file in a formatted buffer of records
type ContentQuestion = {contents: string
                        position: Position}
type ContentFeedback = {mark: string
                        feedback: string
                        position: Position}
type ContentSection = {depth: int
                       title: string
                       points_given: int
                       points_total: int
                       position: Position}

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
let formatHandleError (str: string, linenumber: int) =
    errorHandler (MyFormatException str, linenumber)
    FormattedEmpty

let rec formatFeedback (line: string) (linenumber: int) =
    let pattern = "^[\s]*([\+\?-])[ ]+([\w\W\d\s]+)$"
    if Regex.IsMatch(line, pattern) then
        let res = Regex.Split(line, pattern)
        match res.[1] with
        | "+" -> FormattedFeedback ({mark = mark_feedback_good
                                     feedback = res.[2]
                                     position = linenumber})
        | "-" -> FormattedFeedback ({mark = mark_feedback_bad
                                     feedback = res.[2]
                                     position = linenumber})
        | "?" -> FormattedFeedback ({mark = mark_feedback_unsure
                                     feedback = res.[2]
                                     position = linenumber})
        | _   -> ("Internal error: Unexpected symbol in format feedback",
                  linenumber) |> formatHandleError
    else
        ("Section feedback is malformed", linenumber) |> formatHandleError

let rec formatQuestion (line: string) (linenumber: int) =
    let pattern = "^[\s]*[\*][ ]+([\w\W\d\s]+)$"
    if Regex.IsMatch(line, pattern) then
        let res = Regex.Split(line, pattern)
        FormattedQuestion({contents = res.[1]; position = linenumber})
    else
        ("Section question is malformed", linenumber) |> formatHandleError

let formatSection (line: string) (linenumber: int) =
    let pattern = "^([#]+)[ ]+([\w\d ,-]+): ([\d]*)/([1-9]+[\d]*)$"
    if Regex.IsMatch(line, pattern) then
        let res = Regex.Split(line, pattern)
        FormattedSection({depth = int (res.[1].Length)
                          title = res.[2]
                          points_given = try int (res.[3]) with | _ -> 0
                          points_total = int (res.[4])
                          position = linenumber})
    else
        ("Section header is malformed", linenumber) |> formatHandleError

let rec formatGetType (linenumber: int) = function
    | ""                  -> FormatLineType.Empty
    | StartsWith " " rest -> formatGetType linenumber rest
    | StartsWith "#" rest -> FormatLineType.Section
    | StartsWith "+" rest -> FormatLineType.Feedback
    | StartsWith "-" rest -> FormatLineType.Feedback
    | StartsWith "?" rest -> FormatLineType.Feedback
    | StartsWith "*" rest -> FormatLineType.Question
    | _ ->
        ("Invalid line", linenumber) |> formatHandleError |> ignore
        FormatLineType.Empty

let formatFile (path: string) =
    let file = openFile(path)
    let rec inner(linenumber: int) =
        match readLine(file) with
        | Some s ->
            match formatGetType linenumber s with
            | FormatLineType.Empty    ->
                FormattedContent.FormattedEmpty :: inner(linenumber + 1)
            | FormatLineType.Section  ->
                formatSection s linenumber :: inner(linenumber + 1)
            | FormatLineType.Question ->
                formatQuestion s linenumber :: inner(linenumber + 1)
            | FormatLineType.Feedback ->
                formatFeedback s linenumber :: inner(linenumber + 1)
        | None -> []
    inner(1)


// Parse types:
// Build a recursive tree type from a formatted records buffer
type ParsedQuestion =
    | PQuestion of ContentQuestion * ContentFeedback list

type ParsedSection =
    | PSection of ContentSection * ParsedQuestion list

// allows it to be optional to place a header
type ParsedFile =
    | PFileHeader of ContentSection * ParsedSection list
    | PFileNoHeader of ParsedSection list


// Parse functions:
// Functions to parse a formatted file into a parsed file
let rec parseFile' (file: FormattedContent list) =
    match file with
    | [] -> []
    | (FormattedSection s as x) :: xs ->
        PSection(s, parseSection xs Empty) :: parseFile' xs

    | _ as x :: xs -> parseFile' xs

and parseSection (file: FormattedContent list)
                 (last: FormatLineType) =
    match file with
    | [] -> []
    | (FormattedSection s as x) :: xs ->
        if last = Section then
            let str = "Section is empty"
            warningHandler(MyParseException(str), s.position)
            []
        else []
    | (FormattedQuestion q as x) :: xs ->
        PQuestion(q, parseQuestion xs) :: parseSection xs Question
    | (FormattedFeedback f as x) :: xs ->
        if last = Section then
            let str = "Feedback given without a question, " +
                      "default question will be added"
            warningHandler(MyParseException(str), f.position)
            let question = {contents = "Comments:"; position = f.position}
            PQuestion(question, parseQuestion xs) :: parseSection xs Question
        else parseSection xs last
    | _ as x :: xs -> parseSection xs last

and parseQuestion (file: FormattedContent list) =
    match file with
    | [] -> []
    | (FormattedSection s as x) :: xs -> []
    | (FormattedQuestion q as x) :: xs -> []
    | (FormattedFeedback f as x) :: xs -> f :: parseQuestion xs
    | _ as x :: xs -> parseQuestion xs


// find and return the file header, if such one exists
// if the first item is not a header as FormattedError type will
// be returned as the 'header'. (the caller should check this!)
let rec containsHeader = function
    | ([], _, _) -> Some(FormattedEmpty :: [])
    | (FormattedEmpty :: xs, true, o) -> containsHeader (xs, true, o)
    | (FormattedEmpty :: xs, false, o) -> containsHeader (xs, false, o)
    | (FormattedSection s as x :: xs, true, o) -> o
    | (FormattedSection s as x :: xs, false, o) ->
        containsHeader (xs, true, Some(x :: xs))
    | (_ as x :: xs, false, o) ->
        let str = "The file should start with a section"
        Some(FormattedError(MyIOException(str)) :: [])
    | (_ as x :: xs, true, o) -> containsHeader (xs, true, o)

// wrapper function for parsing a file
let parseFile (file: FormattedContent list) =
    let header = containsHeader (file, false, None)
    match header with
    | Some(lst) ->
        match lst with
        | (FormattedSection x) :: xs -> PFileHeader(x, parseFile' xs)
        | (FormattedEmpty) :: xs ->
            let str = "The file was empty"
            errorHandler (MyIOException(str), 0)
            PFileNoHeader([])
        | (FormattedError e) :: xs ->
            errorHandler(e, 0)
            PFileNoHeader([])
        | _ ->
            let str = "Some error occured"
            errorHandler (MyIOException(str), 0)
            PFileNoHeader([])
    | None          -> PFileNoHeader(parseFile' file)


// HTML-generation
// Functions to generate HTML content
type wrType = string -> unit
type pType = ParsedSection list

let createStart (title: string) (wr: wrType) =
    wr "!DOCTYPE html"
    wr "<html>"
    wr "<head>"
    wr <| "<title>" + title + "</title>"

let rec createSectionIds (parser: pType) (wr: wrType) =
    match parser with
    | [] -> ()
    | PSection(s, xs) :: rest -> ()


and createQuestionIds (parser: pType) (wr: wrType) =
    match parser with
    | PQuestion(q, xs) :: rest -> ()


// HTML-Parsing:
// Convert a parsed file into an html-document
let createHtmlHeader (header: ContentSection, parser: pType) =
    let title = header.title
    let file = writeFile(title)
    let writeLine = fun s -> file.WriteLine("@" + s)
    createStart title writeLine


let createHtmlNoHeader (parser: pType) =
    printf "Enter a name for the output HTML-file: "
    let title = Console.ReadLine()
    ()

let createHtml (parser: ParsedFile) =
    match parser with
    | PFileHeader(sect, lst) -> createHtmlHeader (sect, lst)
    | PFileNoHeader(lst) -> createHtmlNoHeader (lst)
