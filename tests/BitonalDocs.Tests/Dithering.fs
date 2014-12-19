module BitonalDocs.Tests.Dithering

open NUnit.Framework
open FsUnit

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``This test should pass`` () = () |> should equal ()

[<Test>]
let ``This test should fail`` () = () |> should not' (equal ())
