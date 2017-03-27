module HTMLGenerator

open System

#load "types.fsx"
#load "fileio.fsx"
#load "error.fsx"
open Types
open FileIO
open Error


let createHeader (title: string) (wr: wrType) =
    wr "<!DOCTYPE html>"
    wr "<html>"
    wr "<head>"
    wr <| "<title>" + title + "</title>"

let createFooter (wr: wrType) =
    wr "</html>"

let createStyle (wr: wrType) =
    wr "<style>"
    use style = openFile (IO.Path.Combine(__SOURCE_DIRECTORY__,
                                          "html","style.css")) ".css"
    while not <| style.EndOfStream do
        wr <| style.ReadLine()
    wr "</style>"

let createScript (wr: wrType) =
    wr "<script>"
    use script = openFile (IO.Path.Combine(__SOURCE_DIRECTORY__,
                                           "html","script.js")) ".js"
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
    let file = writeFile(fname)
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
    printfn "File '%s' successfully created." fname
