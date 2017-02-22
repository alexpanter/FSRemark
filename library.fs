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

// Extended pattern:
// Allow recursive pattern matching on strings
let (|StartsWith|_|) (s: string) (p: string) =
    if p.StartsWith(s) then Some(p.Substring(s.Length))
    else None


// IO functions:
// Convenient wrappers for file-IO
exception MyIOException of string

let openFile (path: string) = new System.IO.StreamReader(path)

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
    | FormattedSection of ContentSection
    | FormattedQuestion of ContentQuestion
    | FormattedFeedback of ContentFeedback
    | FormattedEmpty
    | FormattedError

type FormattedFile =
    | Lines of FormattedContent list

type FormatLineType =
    | Section
    | Question
    | Feedback
    | Empty

exception MyFormatException of string


// Formatting functions
// Format a file into a structured list of line type records
let rec formatFeedback (line: string) =
    let pattern = "^[\s]*([\+\?-])[ ]+([\w\W\d\s]+)$"
    if Regex.IsMatch(line, pattern) then
        let res = Regex.Split(line, pattern)
        match res.[1] with
        | "+" -> FormattedFeedback({mark = mark_feedback_good;   feedback = res.[2]})
        | "-" -> FormattedFeedback({mark = mark_feedback_bad;    feedback = res.[2]})
        | "?" -> FormattedFeedback({mark = mark_feedback_unsure; feedback = res.[2]})
        | _   -> raise (MyFormatException "Internal error: Unexpected symbol in format feedback")
    else
        raise (MyFormatException "Section feedback is malformed")

let rec formatQuestion (line: string) =
    let pattern = "^[\s]*[\*][ ]+([\w\W\d\s]+)$"
    if Regex.IsMatch(line, pattern) then
        let res = Regex.Split(line, pattern)
        FormattedQuestion({contents = res.[1]})
    else
        raise (MyFormatException "Section question is malformed")

let formatSection (line: string) =
    let pattern = "^([#]+)[ ]+([\w\d ]+): /([\d]+[\d]*)$"
    if Regex.IsMatch(line, pattern) then
        let res = Regex.Split(line, pattern)
        FormattedSection({depth = int (res.[1].Length); title = res.[2]; points = int (res.[3])})
    else
        raise (MyFormatException "Section header is malformed")

let rec formatGetType = function
    | ""                  -> FormatLineType.Empty
    | StartsWith " " rest -> formatGetType rest
    | StartsWith "#" rest -> FormatLineType.Section
    | StartsWith "+" rest -> FormatLineType.Feedback
    | StartsWith "-" rest -> FormatLineType.Feedback
    | StartsWith "?" rest -> FormatLineType.Feedback
    | StartsWith "*" rest -> FormatLineType.Question
    | _ -> raise (MyIOException "Invalid line")

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


// Parsed types:
// Build a recursive tree type from a formatted records buffer
exception MyParseException of string

type ParsedQuestion = FormattedContent * FormattedContent list
type ParsedSection = FormattedContent * ParsedQuestion list

type ParsedFile =
    | ParsedSection of ParsedSection list
    | ParsedEmpty


// Parse functions:
// Parse a formatted file record list into a parsed syntax tree
let parseQuestion = ()

let parseSection = function
    | FormattedSection section -> ()
    | _ -> ()

let rec parseFile (file: FormattedFile) =
    match file with
    | [] -> []
    | (FormattedSection section) :: rest -> (parseSection section) :: parseFile rest
    | _ -> raise (MyParseException "Parse error: file was malformed")

