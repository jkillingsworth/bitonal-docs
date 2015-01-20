module BitonalDocs.Tests.Compression

open NUnit.Framework
open FsUnit
open BitonalDocs.Compression

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Pack bits, compression works correctly`` () =

    let scanline =
        [| 0xAAuy; 0xAAuy; 0xAAuy;
           0x80uy; 0x00uy; 0x2Auy;
           0xAAuy; 0xAAuy; 0xAAuy; 0xAAuy;
           0x80uy; 0x00uy; 0x2Auy; 0x22uy;
           0xAAuy; 0xAAuy; 0xAAuy; 0xAAuy; 0xAAuy; 0xAAuy; 0xAAuy; 0xAAuy; 0xAAuy; 0xAAuy; |]

    let result = compressUsingPackBits scanline
    let expected =
        [| 0xFEuy; 0xAAuy;
           0x02uy; 0x80uy; 0x00uy; 0x2Auy;
           0xFDuy; 0xAAuy;
           0x03uy; 0x80uy; 0x00uy; 0x2Auy; 0x22uy;
           0xF7uy; 0xAAuy; |]

    result |> should equal expected

[<Test>]
let ``Pack bits, matching sequence of 127`` () =

    let scanline = Array.init 127 (fun i -> 0xAAuy)
    let result = compressUsingPackBits scanline
    let expected =
        [| 0x82uy; 0xAAuy; |]

    result |> should equal expected

[<Test>]
let ``Pack bits, matching sequence of 128`` () =

    let scanline = Array.init 128 (fun i -> 0xAAuy)
    let result = compressUsingPackBits scanline
    let expected =
        [| 0x81uy; 0xAAuy; |]

    result |> should equal expected

[<Test>]
let ``Pack bits, matching sequence of 129`` () =

    let scanline = Array.init 129 (fun i -> 0xAAuy)
    let result = compressUsingPackBits scanline
    let expected =
        [| 0x81uy; 0xAAuy; 0x00uy; 0xAAuy; |]

    result |> should equal expected

[<Test>]
let ``Pack bits, matching sequence of 2`` () =

    let scanline = Array.init 2 (fun i -> 0xAAuy)
    let result = compressUsingPackBits scanline
    let expected =
        [| 0x01uy; 0xAAuy; 0xAAuy; |]

    result |> should equal expected

[<Test>]
let ``Pack bits, matching sequence of 3`` () =

    let scanline = Array.init 3 (fun i -> 0xAAuy)
    let result = compressUsingPackBits scanline
    let expected =
        [| 0xFEuy; 0xAAuy; |]

    result |> should equal expected

[<Test>]
let ``Pack bits, non-matching sequence of 127`` () =

    let scanline = Array.init 127 (fun i -> if (i % 2) = 0 then 0xAAuy else 0xBBuy)
    let result = compressUsingPackBits scanline
    let expected =
        Array.concat
             [ [| 0x7Euy; |]; scanline.[000 .. 126]; ]

    result |> should equal expected

[<Test>]
let ``Pack bits, non-matching sequence of 128`` () =

    let scanline = Array.init 128 (fun i -> if (i % 2) = 0 then 0xAAuy else 0xBBuy)
    let result = compressUsingPackBits scanline
    let expected =
        Array.concat
             [ [| 0x7Fuy; |]; scanline.[000 .. 127]; ]

    result |> should equal expected

[<Test>]
let ``Pack bits, non-matching sequence of 129`` () =

    let scanline = Array.init 129 (fun i -> if (i % 2) = 0 then 0xAAuy else 0xBBuy)
    let result = compressUsingPackBits scanline
    let expected =
        Array.concat
             [ [| 0x7Fuy; |]; scanline.[000 .. 127];
               [| 0x00uy; |]; scanline.[128 .. 128]; ]

    result |> should equal expected

[<Test>]
let ``Pack bits, non-matching sequence of 2`` () =

    let scanline = Array.init 2 (fun i -> if (i % 2) = 0 then 0xAAuy else 0xBBuy)
    let result = compressUsingPackBits scanline
    let expected =
        [| 0x01uy; 0xAAuy; 0xBBuy; |]

    result |> should equal expected

[<Test>]
let ``Pack bits, non-matching sequence of 3`` () =

    let scanline = Array.init 3 (fun i -> if (i % 2) = 0 then 0xAAuy else 0xBBuy)
    let result = compressUsingPackBits scanline
    let expected =
        [| 0x02uy; 0xAAuy; 0xBBuy; 0xAAuy; |]

    result |> should equal expected
