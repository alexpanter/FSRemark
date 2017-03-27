module Parser

#load "types.fsx"
#load "fileio.fsx"
#load "error.fsx"
#load "lexer.fsx"

open Types
open FileIO
open Error

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
        obtained := !obtained + s.PointsGiven
        total := !total + s.PointsTotal

        PSection(s, parseSection xs Section s.Depth s.Position)
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
        elif s.Depth > lastDepth then
            let str = "Section hierarchy must be least '"
                      + (getNHashes lastDepth) + "'"
            errorHandler(MyParseException(str), s.Position)
        []

    | (FormattedQuestion q as x) :: xs ->
        PQuestion(q, parseQuestion xs false q.Position)
        :: parseSection xs Question lastDepth lastLine

    | (FormattedFeedback f as x) :: xs ->
        if last = Section then
            let str = "Feedback given without a question, " +
                      "default question will be added"
            warningHandler(MyParseException(str), f.Position)
            let question = {Contents = "Comments:"; Position = f.Position}
            PQuestion(question, parseQuestion xs false f.Position)
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
            headerTotal := x.PointsTotal
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
