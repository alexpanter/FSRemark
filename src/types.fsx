module Types

// Marks:
// mark the beginning of a line
let mark_section         = "#"
let mark_question        = "*"
let mark_feedback_good   = "+"
let mark_feedback_bad    = "-"
let mark_feedback_unsure = "?"


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
type wrType = string -> unit  // append a line (string) to the output file
type pType = ParsedSection list

