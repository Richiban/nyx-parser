module Modules

open FParsec
open Statements
open Common
open Types

let pimportTarget = between (pchar '"') (pchar '"') (manyChars (noneOf ['"'])) .>> spaces |>> ImportTarget

let pimport =
    pipe2
        (keyword "import" .>> spaces >>. commonIdentifier .>> spaces)
        (many pimportTarget)
        (fun _ imports -> imports)

let moduleDefinition =
    pipe3
        (keyword "module" .>> spaces >>. commonIdentifier .>> spaces)
        (opt pimport |>> function None -> [] | Some x -> x)
        (many definitionParser)
        ModuleDefinition.mk

let document: Parser<_, ParserState> = spaces >>. moduleDefinition .>> eof