module Types

// Marks:
// mark the beginning of a line
let markSection         = "#"
let markQuestion        = "*"
let markFeedbackGood   = "+"
let markFeedbackBad    = "-"
let markFeedbackUnsure = "?"


// Extended pattern:
// Allow recursive pattern matching on strings
let (|StartsWith|_|) (s: string) (p: string) =
    if p.StartsWith(s) then Some(p.Substring(s.Length))
    else None


// Position:
// Track the file position (line number) when a syntax error occurs
type Position = int


// Types for error handling:
type MyException =
    | MyIOException     of string
    | MyFormatException of string
    | MyParseException  of string
    | MyHTMLException   of string


// Formatted types:
// Store each line from the file as a record in a formatted list
type ContentQuestion = {Contents: string
                        Position: Position}
type ContentFeedback = {Mark: string
                        Feedback: string
                        Position: Position}
type ContentSection = {Depth: int
                       Title: string
                       PointsGiven: int
                       PointsTotal: int
                       Position: Position}

type ContentMetaAuthor = {Name: string}
type ContentMetaGroup = {Name: string}

type FormattedContent =
    | FormattedSection  of ContentSection
    | FormattedQuestion of ContentQuestion
    | FormattedFeedback of ContentFeedback
    | FormattedEmpty
    | FormattedError of MyException
    | FormattedMetaAuthor of ContentMetaAuthor
    | FormattedMetaGroup of ContentMetaGroup

type FormattedFile =
    | FormattedLines of FormattedContent list

type FormatLineType =
    | Section
    | Question
    | Feedback
    | Empty


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


// HTML-generation
// Functions to generate HTML content
type WrType = string -> unit  // append a line (string) to the output file
type PType = ParsedSection list

