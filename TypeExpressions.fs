module TypeExpressions

open FParsec
open Types
open Common

module private TypeExpressions =
  let typeParser, typeExpressionRef = createParserForwardedToRef()

  let namedFieldParser =
      sepByOp ":" commonIdentifier typeParser 
      |>> NamedField

  let tupleParser =
      parens (choice [
        attempt <| sepByCommas1 namedFieldParser 
        |>> Tuple
        
        typeParser .>> spaces .>> pchar ',' .>> spaces .>>. sepByCommas1 typeParser
        |>> (fun (x, xs) -> Tuple (x::xs))
      ])

  // let typeCall = 
  //   (typeExpression .>> spaces) .>>. (typeExpression .>> spaces)
  //   |>> TypeCall

  let tag =
    pstring "#" >>. commonIdentifier |>> Tag


  let typeTerm = spaced <| choice [
    typeIdentifier |>> Identifier
    namedFieldParser
    tag
  ]


  let typeOperators = choice [
      pstring "|" >>% (fun x y -> Union (x, y))
      pstring "&" >>% (fun x y -> Intersection (x, y))
      pstring "->" >>% (fun x y -> FunctionType (x, y))
  ]

  let term = choice [
      attempt <| (spaced <| (pchar '(' .>> spaces .>> pchar ')') |>> (fun _ -> Unit))
      attempt <| (spaced <| tupleParser)
      attempt <| (spaced <| parens (spaced typeParser))
      typeTerm
  ]

  do typeExpressionRef.Value <- chainl1 term typeOperators

let typeParser = TypeExpressions.typeParser