module NyxParser.Tests.ModuleDefinitions

open NUnit.Framework

open NyxParser.Modules
open NyxParser.Types
open NyxParser.Tests.Utils


[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Test module definition with imports``() =
    let actual = 
        runParser moduleParser @"module Test

import
    ""test1""
    ""test2""
    test3
"
    let expected = { 
        name = ModuleIdentifier "Test"
        imports = Some (ImportSection [ImportTarget "test1"; ImportTarget "test2"; ImportTarget "test3"])
        definitions = []
    }

    Assert.AreEqual(expected, actual)


