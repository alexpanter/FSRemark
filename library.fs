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


// Types for error handling:
type MyException =
    | MyIOException     of string
    | MyFormatException of string
    | MyParseException  of string
    | MyHTMLException   of string

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


// Extended pattern:
// Allow recursive pattern matching on strings
let (|StartsWith|_|) (s: string) (p: string) =
    if p.StartsWith(s) then Some(p.Substring(s.Length))
    else None


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


// Formatted types:
// Store each line from the file as a record in a formatted list
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
    let file = openFile path ".mrk"
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
// Functions to parse a formatted file into a parsed file.
// Parameters:
// 'file' - the formatted (lexer) list to be parsed
// 'total' - will contain the total maximum number of points
// 'obtained' - will contain the total obtained number of points
let rec parseFile' (file: FormattedContent list) (obtained: int ref)
                   (total: int ref) =
    match file with
    | [] -> []
    | (FormattedSection s as x) :: xs ->
        obtained := !obtained + s.points_given
        total := !total + s.points_total

        PSection(s, parseSection xs Section s.depth s.position)
        :: parseFile' xs obtained total

    | _ as x :: xs -> parseFile' xs obtained total

and parseSection (file: FormattedContent list) (last: FormatLineType)
                 (lastDepth: int) (lastLine: int) =
    let rec getNHashes = function
        | 0 -> ""
        | n -> "#" + getNHashes (n - 1)
    let notifyEmptySection() =
        let str = "Section is empty"
        warningHandler(MyParseException(str), lastLine)

    match file with
    | [] -> (if last = Section then notifyEmptySection()); []

    | (FormattedSection s as x) :: xs ->
        if last = Section then
            notifyEmptySection()
        elif s.depth > lastDepth then
            let str = "Section hierarchy must be least '"
                      + (getNHashes lastDepth) + "'"
            errorHandler(MyParseException(str), s.position)
        []

    | (FormattedQuestion q as x) :: xs ->
        PQuestion(q, parseQuestion xs false q.position)
        :: parseSection xs Question lastDepth lastLine

    | (FormattedFeedback f as x) :: xs ->
        if last = Section then
            let str = "Feedback given without a question, " +
                      "default question will be added"
            warningHandler(MyParseException(str), f.position)
            let question = {contents = "Comments:"; position = f.position}
            PQuestion(question, parseQuestion xs false f.position)
            :: parseSection xs Question lastDepth lastLine
        else parseSection xs last lastDepth lastLine

    | _ as x :: xs -> parseSection xs last lastDepth lastLine

and parseQuestion (file: FormattedContent list) (answered: bool) (lastLine: int) =
    let questionNotAnswered() =
        let str = "Question has not been answered"
        warningHandler(MyParseException(str), lastLine)
    match file with
    | [] -> []
    | FormattedSection s as x :: xs    ->
        (if not answered then questionNotAnswered ()); []
    | FormattedQuestion q as y :: ys   ->
        (if not answered then questionNotAnswered ()); []
    | (FormattedFeedback f as x) :: xs -> f :: parseQuestion xs true lastLine
    | _ as x :: xs -> parseQuestion xs answered lastLine


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
    let headerTotal = ref 0
    let total = ref 0
    let obtained = ref 0
    let header = containsHeader (file, false, None)
    match header with
    | Some(lst) ->

        match lst with
        | (FormattedSection x) :: xs ->
            headerTotal := x.points_total
            let pFile = PFileHeader(x, parseFile' xs obtained total)
            if !obtained > !headerTotal then
                let str = sprintf "Points don't add up: (%i/%i)"
                              !obtained !headerTotal
                errorHandler(MyParseException(str), 1)
            if !total <> !headerTotal then
                let str = "Total points calculated wrongly, " +
                          "expected " + (string !headerTotal) +
                          " but got " + (string !total)
                errorHandler(MyParseException(str), 1)
            (pFile, obtained, total)

        | (FormattedEmpty) :: xs ->
            let str = "The file was empty"
            errorHandler (MyIOException(str), 0)
            (PFileNoHeader([]), ref 0, ref 0) // will never return

        | (FormattedError e) :: xs ->
            errorHandler(e, 0)
            (PFileNoHeader([]), ref 0, ref 0) // will never return

        | _ ->
            let str = "Some error occured"
            errorHandler (MyIOException(str), 0)
            (PFileNoHeader([]), ref 0, ref 0) // will never return

    | None ->
        let pFile = PFileNoHeader(parseFile' file obtained total)
        if !obtained > !total then
            let str = sprintf "Points don't add up: (%i/%i)" !obtained !total
            errorHandler(MyParseException(str), 1)
        (pFile, obtained, total)


// HTML-generation
// Functions to generate HTML content
type wrType = string -> unit  // append a line (string) to the output file
type pType = ParsedSection list

let createHeader (title: string) (wr: wrType) =
    wr "<!DOCTYPE html>"
    wr "<html>"
    wr "<head>"
    wr <| "<title>" + title + "</title>"

let createFooter (wr: wrType) =
    wr "</html>"

let createStyle (wr: wrType) =
    wr "<style>"
    use style = openFile (IO.Path.Combine("html","style.css")) ".css"
    while not <| style.EndOfStream do
        wr <| style.ReadLine()
    wr "</style>"

let createScript (wr: wrType) =
    wr "<script>"
    use script = openFile (IO.Path.Combine("html","script.js")) ".js"
    while not <| script.EndOfStream do
        wr <| script.ReadLine()
    wr "</script>"

let rec createBody (parser: pType) (wr: wrType) =
    let rec appendSection = function
        | [] -> ()
        | PSection(s, ql) :: xs ->
            wr "<div show=\"0\">"
            wr "<button class=\"button bt-section\" onclick=\"expandSection(this)\">"
            wr s.title
            wr "</button><div>"
            appendQuestion ql
            wr "</div></div>"
            appendSection xs

    and appendQuestion = function
        | [] -> ()
        | PQuestion(q, fl) :: xs ->
            wr "<div show=\"0\">"
            wr "<button class=\"button bt-question\" onclick=\"expandQuestion(this)\">"
            wr q.contents
            wr "</button><div>"
            appendFeedback fl
            wr "</div></div>"
            appendQuestion xs

    and appendFeedback = function
        | [] -> ()
        | x :: xs ->
            wr "<div class=\"mark-container\"><button class=\"mark "
            match x.mark with
                | "+" -> wr "mark-good\">"
                | "-" -> wr "mark-bad\">"
                | "?" -> wr "mark-unsure\">"
                | _ -> errorHandler(MyHTMLException("Invalid feedback mark!"), 0)
            wr x.mark
            wr "</button>"
            wr x.feedback
            wr "</div>"
            appendFeedback xs

    appendSection parser


// HTML-Parsing:
// Convert a parsed file structure into an html-document,
// and stream it into a file.
let htmlParser (parser: ParsedFile, obtained: int ref, total: int ref) =
    let mutable title = ""
    let mutable fname = ""
    let mutable header = Unchecked.defaultof<ContentSection>
    let mutable parseList = Unchecked.defaultof<ParsedSection list>

    match parser with
    | PFileHeader(sect, lst) ->
        printf "Enter a name for the output HTML-file (without '.html'): "
        fname <- (Console.ReadLine() + ".html").Replace(" ", "")
        title <- sect.title
        header <- sect
        parseList <- lst
    | PFileNoHeader(lst) ->
        printf "Enter a name for the assignment: "
        fname <- Console.ReadLine()
        printf "Enter a name for the output HTML-file: "
        title <- Console.ReadLine()
        header <- {depth = -1; title = title; points_given = -1
                   points_total = -1; position = -1}
        parseList <- lst

    // file stream
    let file = writeFile(IO.Path.Combine("out", fname))
    let wr = string >> file.WriteLine

    // header
    createHeader fname wr
    createStyle wr
    wr "</head>"

    // body
    wr "<body>"
    wr <| "<h1>" + title + "</h1><hr>"
    createBody parseList wr
    wr <| sprintf "<hr><h4>Points in total: %i/%i</h4>" !obtained !total
    wr "</body>"

    // script and footer
    createScript wr
    wr "</html>"

    // end of parse.
    file.Close()
    printfn "File %s successfully created." <| "out/" + fname
