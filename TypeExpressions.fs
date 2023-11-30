module TypeExpressions

open FParsec
open Types
open Common


let typeExpression, typeExpressionRef = createParserForwardedToRef()

let objectFieldParser =
  choice [
    sepByOp ":" commonIdentifier typeExpression
    |>> (fun (name, typeExpression) -> ObjectField.mk (Some name) typeExpression)
    typeExpression |>> (ObjectField.mk None)
  ]

let objectTupleParser =
    (sepBySemis1 objectFieldParser)
    |>> ObjectTuple

let typeCall = 
  (typeExpression .>> spaces) .>>. (typeExpression .>> spaces)
  |>> TypeCall

let tag =
  pstring "#" >>. commonIdentifier |>> Tag


let atom = spaced <| choice [
  typeIdentifier |>> Identifier
  //(separatedByCommas objectFieldParser) |>> (fun x -> ObjectTuple x)
  tag
]

// let crossop =
//     pstring "*" >>% (fun x y -> ObjectTuple [x; y])

let ops = choice [
    //crossop
    pstring "|" >>% (fun x y -> Union (x, y))
    pstring "&" >>% (fun x y -> Intersection (x, y))
    pstring "->" >>% (fun x y -> FunctionType (x, y))
]

let term = choice [
    attempt <| (spaced <| (pchar '(' .>> spaces .>> pchar ')') |>> (fun _ -> Unit))
    attempt <| (spaced <| parens objectTupleParser)
    attempt <| (spaced <| parens (spaced typeExpression))
    atom
]

do typeExpressionRef.Value <- chainl1 term ops