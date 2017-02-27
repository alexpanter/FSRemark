# FSharpRemarks

### Summary:
A humble attempt at recreating the Remarks application:
[DIKU-EDU/remarks](https://github.com/DIKU-EDU/remarks "DIKU-EDU/remarks")

### Motivation:
This project aims to ease the parsing of remark files by utilizing the built-in
string functionalities provided by F# and the .NET platform.

In contrary to the original `remarks` application, `FSharpRemarks` is designed
to be a simple console tool which, given a remarks (.mrk) file as input
produces an html document, no questions asked.

__To attain these goals__ :
* All syntax errors in a `remarks` file are reported with an exact line number
  and a short description of the error.
* The application will automatically calculate the student's points in total,
  and add them to the html file.
* If the maximum number of points for each task do not add up to the provided
  total, an error will be printed with the difference.
* In contrast to the original design, the header line is now optional.


### Technicalities:
The application works by first generating a list of lexer tokens from the file,
ie. a list of struct unions (in proper `F#` lingo).

This list is then run through a parser which builds a syntax tree for the file
using list-recursion.

Finally, the created syntax tree is parsed into a html, and streamed to a target file,
together with some css and javascript.

### Credits:
_This project is by no account officially related to Remarks_.

All credits go to the amazing people at [DIKU-EDU](https://github.com/DIKU-EDU "DIKU-EDU")
who came up with the concept and formulization of the remarks (`.mrk`) format.

Pull-requests and posting of issues are welcome!

