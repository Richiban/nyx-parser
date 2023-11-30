module TypeExpressions

open FParsec
open Types
open Common

type TP = Parser<TypeExpression, IndentationState>

let (typeParser: TP), typeExpressionRef = createParserForwardedToRef()

let namedFieldParser =
    sepByOp ":" commonIdentifier typeParser

let objectTupleParser =
    parens (sepByCommas1 typeParser)
    |>> Tuple

// let typeCall = 
//   (typeExpression .>> spaces) .>>. (typeExpression .>> spaces)
//   |>> TypeCall

let tag =
  pstring "#" >>. commonIdentifier |>> Tag


let atom = spaced <| choice [
  typeIdentifier |>> Identifier
  namedFieldParser |>> NamedField
  tag
]


let ops = choice [
    pstring "|" >>% (fun x y -> Union (x, y))
    pstring "&" >>% (fun x y -> Intersection (x, y))
    pstring "->" >>% (fun x y -> FunctionType (x, y))
]

let term = choice [
    attempt <| (spaced <| (pchar '(' .>> spaces .>> pchar ')') |>> (fun _ -> Unit))
    attempt <| (spaced <| objectTupleParser)
    attempt <| (spaced <| parens (spaced typeParser))
    atom
]

do typeExpressionRef.Value <- chainl1 term ops