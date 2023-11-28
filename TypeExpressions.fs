module TypeExpressions

open FParsec
open Types
open Common

let builtInTypes = ["int"; "float"; "string"; "list"]

let customType =
    let isAsciiIdStart    = fun c -> isAsciiUpper c 
    let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_'

    identifier (IdentifierOptions(
                    isAsciiIdStart = isAsciiIdStart,
                    isAsciiIdContinue = isAsciiIdContinue,
                    normalization = System.Text.NormalizationForm.FormKC,
                    normalizeBeforeValidation = true,
                    allowAllNonAsciiCharsInPreCheck = true)) |>> CustomType

let builtInType = choice (builtInTypes |> List.map pstring) |>> BuiltInType

let typeName = spaces >>. choice [customType; builtInType] .>> spaces

let high = OperatorPrecedenceParser<TypeExpression, unit, UserState>()
let low = OperatorPrecedenceParser<TypeExpression, unit, UserState>()
let pHighExpr = high.ExpressionParser .>> spaces
let pLowExpr: Parser<_, UserState> = low.ExpressionParser .>> spaces

high.TermParser <-
  choice
    [ typeName
      pstring "()" |>> fun _ -> Unit
      between (str "(") (str ")") pLowExpr ]

low.TermParser <-
  many1 pHighExpr |>> (function [f] -> f | fs -> List.reduce (fun f g -> TypeCall(f, g)) fs) .>> spaces

low.AddOperator(InfixOperator("|", spaces, 10, Associativity.Left, fun f g -> Union(f, g)))
high.AddOperator(InfixOperator("&", spaces, 20, Associativity.Left, fun f g -> Intersection(f, g)))
high.AddOperator(InfixOperator("->", spaces, 20, Associativity.Left, fun f g -> FunctionType(f, g)))
high.AddOperator(InfixOperator(",", spaces, 20, Associativity.Left, fun a b -> Tuple([a; b])))

let typeExpression: Parser<_, UserState> = pLowExpr