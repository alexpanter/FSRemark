module Lexer

open System.Text.RegularExpressions

#load "types.fsx"
#load "error.fsx"
#load "fileio.fsx"
open Types
open Error
open FileIO


// Formatting functions
// Format a file into a structured list of line type records
let formatHandleError (str: string, linenumber: int) =
    errorHandler (MyFormatException str, linenumber)
    FormattedEmpty

let rec formatFeedback (line: string) (linenumber: int) =
    let pattern = "^[\s]*([\+\?-])[ ]+([\w\W\d\s\":;\(\)\{\}\[\]\-\^<>.,]+)$"
    if Regex.IsMatch(line, pattern) then
        let res = Regex.Split(line, pattern)
        match res.[1] with
        | "+" -> FormattedFeedback ({Mark = markFeedbackGood
                                     Feedback = res.[2]
                                     Position = linenumber})
        | "-" -> FormattedFeedback ({Mark = markFeedbackBad
                                     Feedback = res.[2]
                                     Position = linenumber})
        | "?" -> FormattedFeedback ({Mark = markFeedbackUnsure
                                     Feedback = res.[2]
                                     Position = linenumber})
        | _   -> ("Internal error: Unexpected symbol in format feedback",
                  linenumber) |> formatHandleError
    else
        ("Section feedback is malformed", linenumber) |> formatHandleError

let rec formatQuestion (line: string) (linenumber: int) =
    let pattern = "^[\s]*[\*][ ]+([\w\W\d\s]+)$"
    if Regex.IsMatch(line, pattern) then
        let res = Regex.Split(line, pattern)
        FormattedQuestion({Contents = res.[1]; Position = linenumber})
    else
        ("Section question is malformed", linenumber) |> formatHandleError

let formatSection (line: string) (linenumber: int) =
    let pattern = "^([#]+)[ ]+([\w\d ,-]+): ([\d]*)/([\d]+)$"
    if Regex.IsMatch(line, pattern) then
        let res = Regex.Split(line, pattern)
        FormattedSection({Depth = int (res.[1].Length)
                          Title = res.[2]
                          PointsGiven = try int (res.[3]) with | _ -> 0
                          PointsTotal = int (res.[4])
                          Position = linenumber})
    else
        ("Section header is malformed", linenumber) |> formatHandleError

let formatMeta (line: string) (linenumber: int) =
    let castError() =
        ("Meta content is malformed", linenumber) |> formatHandleError
    let pattern = "^.([\w]+): ([\w ]+)$"
    if Regex.IsMatch(line, pattern) then
        let res = Regex.Split(line, pattern)
        match res.[1] with
            | "author" -> FormattedMetaAuthor({Name = res.[2]
                                               Position = linenumber})
            | "group" -> FormattedMetaGroup({Name = res.[2]
                                             Position = linenumber})
            | _ -> castError()
    else
        castError()


let rec formatGetType (linenumber: int) = function
    | ""                  -> FormatLineType.Empty
    | StartsWith " " rest -> formatGetType linenumber rest
    | StartsWith "#" rest -> FormatLineType.Section
    | StartsWith "+" rest -> FormatLineType.Feedback
    | StartsWith "-" rest -> FormatLineType.Feedback
    | StartsWith "?" rest -> FormatLineType.Feedback
    | StartsWith "*" rest -> FormatLineType.Question
    | StartsWith "." rest -> FormatLineType.Meta
    | _ ->
        ("Invalid line", linenumber) |> formatHandleError |> ignore
        FormatLineType.Empty // dummy return value

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
            | FormatLineType.Meta     ->
                formatMeta s linenumber :: inner(linenumber + 1)
        | None -> []
    inner(1)
