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
      (choice [
        attempt <| sepByCommas1 namedFieldParser 
        |>> Tuple
        
        typeParser .>> spaces .>> pchar ',' .>> spaces .>>. sepByCommas1 typeParser
        |>> (fun (x, xs) -> Tuple (x::xs))
      ])
  
  /// Requires the given parser to be surrounded by parentheses only if the parser state
  /// is not already in parentheses
  let parensNotDoubled (p: Parser<'t>) : Parser<_> =
    fun stream ->
        let inParens = stream.UserState.CurrentlyInParens

        if inParens then
            stream.UserState <- {stream.UserState with CurrentlyInParens = false}
            let reply = p stream
            reply
        else
            stream.UserState <- {stream.UserState with CurrentlyInParens = true}
            let reply = (parens p) stream
            reply

  let tag =
    pstring "#" >>. commonIdentifier |>> Tag


  let typeOperators = choice [
      pstring "|" >>% (fun x y -> Union (x, y))
      pstring "&" >>% (fun x y -> Intersection (x, y))
      pstring "->" >>% (fun x y -> FunctionType (x, y))
  ]

  let term = spaced <| choice [
      attempt <| (typeIdentifier .>> spaces .>>. parensNotDoubled typeParser) |>> (fun (x, y) -> TypeCall(Identifier x, y))
      attempt <| ((pchar '(' .>> spaces .>> pchar ')') |>> (fun _ -> Unit))
      attempt <| (parensNotDoubled tupleParser)
      attempt <| (parens (spaced typeParser))
      
      attempt typeIdentifier |>> Identifier
      attempt namedFieldParser
      tag
  ]

  do typeExpressionRef.Value <- chainl1 term typeOperators

let typeParser = TypeExpressions.typeParser