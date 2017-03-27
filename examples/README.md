# LAYOUT

A `remarks` file consists of sections, questions, feedback's, and meta contens.

### Example file:

```fsharp
# Assignment 1 - Design Patterns: /100                    // total points given for the assignment
.author: King Arthur                                      // meta information
.author: Lancelot
.group: Knights of the Round Table

## Task 1 - Singleton: /40                                // this task is 40/100 points
  * Understands the <tt>Singleton</tt> Design Pattern     // question, used for giving feedback
    + Yes, good answering in report                       // positive feedback given for a question

## Task 3 - Strategy: /40
  * Understands the <tt>Strategy</tt> Design Pattern
    - Not quite                                           // also feedback, but negative
    ? The code implementation seems off                   // feedback, unsure what to answer

## Report: /20
  * Well-written
    + ...

## Comments: /0                                           // sections can be used more freely (doesn't add points)
  * Code
    - Some mistakes
    + Code is nice
    + ...
  * Report
    - Missing conclusion
    - ...
```
