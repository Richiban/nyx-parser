module Types
type Identifier = string

type Expression =
    | StringLiteral of string
    | IntLiteral of int
    | Block of Expression list

type TypeExpression =
    | BuiltInType of string
    | CustomType of string
    | Intersection of TypeExpression * TypeExpression
    | Union of TypeExpression * TypeExpression
    | FunctionType of TypeExpression * TypeExpression
    | TypeCall of TypeExpression * TypeExpression
    | Tuple of TypeExpression list
    | Unit

type FunctionArgument = {
    name: string
    argumentType: TypeExpression option 
}

and VariableDefinition = { name: string; value: Block }

and FunctionDefinition = { 
    name: string
    arguments: FunctionArgument list
    body: Block 
}

and Definition = 
    | V of VariableDefinition
    | F of FunctionDefinition

and Statement = 
    | Def of Definition
    | Expr of Expression

and Block = Block of Statement list

type ImportTarget = ImportTarget of string

type ModuleDefinition = { name: string; imports: ImportTarget list; definitions: Definition list }

type LastParsedIndentation() =
    [<DefaultValue>]
    val mutable Value: int32
    [<DefaultValue>]
    val mutable EndIndex: int64

type UserState = 
    {   Indentation: int
        // We put LastParsedIndentation into the UserState so that we 
        // can conveniently use a separate instance for each stream.
        // The members of the LastParsedIndentation instance will be mutated
        // directly and hence won't be affected by any stream backtracking. 
        LastParsedIndentation: LastParsedIndentation }
    with
        static member Create() = 
            { Indentation = -1
              LastParsedIndentation = LastParsedIndentation(EndIndex = -1L) }