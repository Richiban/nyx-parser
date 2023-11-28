module Modules

open FParsec
open Statements
open Common
open Types

let pimportTarget = commonIdentifier .>> spaces |>> ImportTarget

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
        (fun name imports definitions -> { name = name; imports = imports; definitions = definitions })

let document: Parser<_, UserState> = spaces >>. moduleDefinition .>> eof