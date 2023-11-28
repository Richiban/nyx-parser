module Main 

open FParsec
open Types
open Common
open TypeExpressions
open Modules 
open Statements

runParserOnString typeExpression (UserState.Create()) "" "A((B, C) -> C)"
|> printfn "%A"

runParserOnString definitionParser (UserState.Create()) "" @"def foo(x: string()) -> 1"
|> printfn "%A"

runParserOnString document (UserState.Create()) "" @"
module foo

def foo(x: string()) -> 
    1

def foo(x: string()) -> 
    2
"
|> printfn "%A"