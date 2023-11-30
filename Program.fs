module Main 

open FParsec
open Types
open Common
open TypeExpressions
open Modules 
open Statements

let runParser p = runParserOnString (p .>> spaces .>> eof) (IndentationState.Create()) ""

// runParser typeDefinitionParser "type A = B -> C"
// |> printfn "%A"

runParser 
    typeDefinitionParser
    "type A = (filename: string; mode: #readonly | #readwrite) -> ()"
|> printfn "%A"

// runParser typeExpression "(#nil | #other)"
// |> printfn "%A"

// runParser typeExpression "() -> (i: int, s: (string))"
// |> printfn "%A"

// runParser definitionParser @"def foo(x: string()) -> 1"
// |> printfn "%A"

// runParser document @"
// module foo

// def x = ""Hello world""

// def foo(x: list(string)) -> 
//     1

// def foo(x: string()) -> 
//     2
// "
// |> printfn "%A"


// type Expression =
//     | Identifier of string
//     | Call of Expression * Expression list
//     | And of Expression * Expression
//     | Or of Expression * Expression



// let identifier = many1Satisfy isLetter

// let (expr: Parser<Expression, unit>), exprRef = createParserForwardedToRef()

// let parens p = between (pchar '(' .>> spaces) (spaces >>. pchar ')') p

// let commaSeparated p = sepBy p (pchar ',' .>> spaces)

// let callExpr = 
//     pipe2 
//         (identifier .>> spaces) 
//         (parens (commaSeparated expr)) 
//         (fun name args -> Call(Identifier name, args))

// let atomExpr = 
//     choice [
//         //callExpr;
//         identifier |>> Identifier;
//         parens expr
//     ]

// let opp = operators [
//     "|", spaces, 10, Associativity.Left, fun l r -> Or(l, r)
//     "&", spaces, 20, Associativity.Left, fun l r -> And(l, r)
// ]

// exprRef.Value <- attempt atomExpr <|> opp.ExpressionParser

// run expr "(A & B) | F(C)"
// |> printfn "%A"

// // Expected: Or (And (Identifier "A", Identifier "B"), Call ("F", [Identifier "C"]))


// open FParsec

// type TypeDefinition =
//     { name: string
//       value: TypeValue }

// and TypeValue =
//     | TypeIdentifier of CustomType
//     | TypeCall of TypeValue * TypeValue
//     | Tuple of Field list
//     | Union of TypeValue * TypeValue
//     | Intersection of TypeValue * TypeValue
//     | FunctionType of TypeValue * TypeValue

// and Field =
//     | AnonField of TypeValue
//     | NamedField of NamedField

// and AnonField =
//     { fieldType: TypeValue }

// and NamedField =
//     { name: string
//       fieldType: TypeValue }

// and CustomType = string
// and BuiltInType = string

// let identifier = many1Satisfy isLetter
// let customType = identifier |>> CustomType
// let builtInType: Parser<_, unit> = pstring "string" |>> BuiltInType
// let keyword str = pstring str .>> spaces
// let typeKeyword = keyword "type" .>> spaces >>. identifier
// let spaceOp c = spaces >>. pchar c >>. spaces

// let typeIdentifier = customType |>> TypeIdentifier

// let typeExpression, typeExpressionRef = createParserForwardedToRef()

// let typeCall = 
//     pipe2 typeExpression (parens typeExpression)
//         (fun t v -> TypeCall (t, v))

// let namedField = 
//     identifier .>> spaceOp ':' .>>. typeExpression
//     |>> fun (n, t) -> { name = n; fieldType = t }

// let tuple = choice [
//     attempt <| commaSeparated namedField |>> (List.map NamedField >> Tuple)
//     typeIdentifier .>> spaceOp ',' .>>. many typeIdentifier |>> (fun (a, b) -> Tuple ((AnonField a) :: (b |> List.map AnonField)))
// ]

// let ops = operators [
//     "|", spaces, 10, Associativity.Left, fun l r -> Union(l, r)
//     "&", spaces, 20, Associativity.Left, fun l r -> Intersection(l, r)
//     "->", spaces, 30, Associativity.Left, fun a b -> FunctionType(a, b)
// ]

// ops.TermParser <- typeExpression

// do typeExpressionRef.Value <- 
//     choice [ 
//         attempt <| parens ops.ExpressionParser 
//         attempt <| parens typeExpression
//         attempt tuple
//         //typeCall
//         typeIdentifier
//     ]

// let typeDefinition = 
//     (typeKeyword) .>> (spaceOp '=') .>>. typeExpression 
//     |>> (fun (n, v) -> { name = n; value = v })

// //let input = "type A = left: C, middle: C, right: string"
// let input = "type A = (string -> int)"

// run typeDefinition input 
// |> printfn "%A"
