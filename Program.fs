module Main 

open FParsec
open Types
open Common
open TypeExpressions
open Modules 
open Statements

let runParser p s = 
    runParserOnString (p .>> spaces .>> eof) (IndentationState.Create()) "" s
    |> printfn "%A"

// runParser 
//     typeDefinitionParser
//     "type A = (filename: string; mode: #readonly | #readwrite) -> ()"

// runParser 
//     typeDefinitionParser
//     "type OptionString = #none | (#some, string)"

// runParser
//     typeParser
//     "#none | (#some, string) -> ()"

runParser
    typeParser
    "(a:string) -> (string) -> (string, string)"


// runParser 
//     document
//     @" 
//     module foo
//     def message = ""Hello world""
//     def f(x: int) -> x + 1"