﻿module Main 

open FParsec
open Types
open Common
open TypeExpressions
open Modules 
open Statements

let runParser p s = 
    runParserOnString (p .>> spaces .>> eof) (ParserState.Create()) "" s
    |> printfn "%A"

// runParser 
//     typeDefinitionParser
//     "type A = (filename: string, mode: #readonly | #readwrite) -> ()"

// runParser 
//     typeDefinitionParser
//     "type OptionString = (#none | (#some, string))"

// runParser
//     typeParser
//     "#none | (#some, string) -> ()"

// runParser
//     typeParser
//     "string -> list(string, int)"
    

// runParser
//     typeParser
//     "string -> list(string)"

// runParser
//     typeParser
//     "string -> list string" // This _should_ fail

// runParser
//     typeParser
//     "string -> (string)"

// runParser
//     typeParser
//     "(((string)))"


runParser 
    importSectionParser
    @"import 
    ""test1""
    ""test2""
"

runParser
    moduleParser
    @"module Test

import
    ""test1""
    ""test2""
    test3
"