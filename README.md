# FSharpRemarks

### Summary:
A humble attempt at recreating the Remarks application:
[DIKU-EDU/remarks](https://github.com/DIKU-EDU/remarks "DIKU-EDU/remarks")

### Motivation:
This project aims to ease the parsing of remark files by utilizing the built-in
string functionalities provided by F# and the .NET platform.

All syntax errors in a `remarks` file are reported with an exact line number
and a short description of the error. This is motivated by the need for
more user-friendly feedback in the original application.

### Technicalities:
The application works by first generating a list of lexer tokens from the file,
ie. a list of struct unions (in proper `F#` lingo).

This list is then run through a parser which builds a syntax tree for the file
using list-recursion.

__Not yet implemented :__

Another parser that transforms the syntax tree into a styled `html`-document will come soon.

### Credits:
_This project is by no account officially related to Remarks_.

All credits go to the amazing people at [DIKU-EDU](https://github.com/DIKU-EDU "DIKU-EDU")
who came up with the concept and formulization of the remarks (`.mrk`) format.

Pull-requests and posting of issues are welcome!

