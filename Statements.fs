module Statements

open FParsec
open Common
open TypeExpressions
open Types


let pstringLiteral = between (pchar '"') (pchar '"') (manyChars (noneOf ['"'])) |>> StringLiteral

let pintLiteral =
    many1Satisfy isDigit
    |>> int
    |>> IntLiteral


let pexpression =
    choice [attempt pstringLiteral; pintLiteral]

let blockParser, blockParserRef = createParserForwardedToRef()

let functionArgument: Parser<_, IndentationState> = 
    commonIdentifier .>> spaces .>>.
        (opt (pchar ':' .>> spaces >>. typeParser))
        |>> FunctionArgument.mk

let argumentListParser: Parser<_, IndentationState> =
    parens (sepByCommas functionArgument)

let functionDefinitionParser: Parser<_, IndentationState> =
    pipe3
        (keyword "def" .>> spaces >>. commonIdentifier .>> spaces)
        (argumentListParser .>> spaces .>> pstring "->" .>> spaces)
        (blockParser)
        (fun x y z -> FunctionDefinition.mk(x, y, z))

let valueBindingParser =
    (keyword "def" .>> spaces >>. commonIdentifier .>> spaces)
    .>>. (pchar '=' .>> spaces .>> opt newline >>. blockParser)
    |>> ValueDefinition.mk

let typeDefinitionParser =
    (keyword "type" .>> spaces >>. typeIdentifier .>> spaces)
    .>>. (pchar '=' .>> spaces >>. typeParser)
    |>> TypeDefinition.mk

let definitionParser: Parser<_, IndentationState> =
    choice [
        attempt functionDefinitionParser |>> Func
        attempt typeDefinitionParser |>> Type
        valueBindingParser |>> Val
    ]

let pstatement = choice [attempt definitionParser |>> Def; pexpression |>> Expr]

do blockParserRef.Value <- indentedMany1 pstatement "block" |>> Block .>> wsBeforeEOL