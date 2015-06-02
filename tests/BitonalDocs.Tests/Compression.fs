module BitonalDocs.Tests.Compression

open NUnit.Framework
open FsUnit
open BitonalDocs.Types
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

//-------------------------------------------------------------------------------------------------

let private group3StringToPixels value =
    value
    |> Seq.map (fun i -> if i = '0' then Black else White)
    |> Seq.toArray

let private group3StringFromBits value =
    value
    |> Seq.map (fun i -> if i = Bit0 then '0' else '1')
    |> Seq.toArray
    |> (fun x -> new string(x))

let private group3Test scanline =
    scanline
    |> group3StringToPixels
    |> compressUsingGroup3
    |> group3StringFromBits
    |> should equal

[<Test>]
let ``Group 3, compression works correctly`` () =

    let expected = "00110101" + "10" + "000111" + "11" + "0111" + "010" + "1000"
    let scanline = "000100110111"
    group3Test scanline expected

[<Test>]
let ``Group 3, empty sequence of 0`` () =

    let expected = "00110101"
    let scanline = ""
    group3Test scanline expected

[<Test>]
let ``Group 3, matching black sequence of 1`` () =

    let expected = "00110101" + "010"
    let scanline = "0"
    group3Test scanline expected

[<Test>]
let ``Group 3, matching black sequence of 2`` () =

    let expected = "00110101" + "11"
    let scanline = "00"
    group3Test scanline expected

[<Test>]
let ``Group 3, matching black sequence of 3`` () =

    let expected = "00110101" + "10"
    let scanline = "000"
    group3Test scanline expected

[<Test>]
let ``Group 3, matching black sequence of 63`` () =

    let expected = "00110101" + "000001100111"
    let scanline = Array.create 63 '0'
    group3Test scanline expected

[<Test>]
let ``Group 3, matching black sequence of 64`` () =

    let expected = "00110101" + "0000001111" + "0000110111"
    let scanline = Array.create 64 '0'
    group3Test scanline expected

[<Test>]
let ``Group 3, matching black sequence of 65`` () =

    let expected = "00110101" + "0000001111" + "010"
    let scanline = Array.create 65 '0'
    group3Test scanline expected

[<Test>]
let ``Group 3, matching black sequence of 700`` () =

    let expected = "00110101" + "0000001001010" + "000000101100"
    let scanline = Array.create 700 '0'
    group3Test scanline expected

[<Test>]
let ``Group 3, matching black sequence of 7000`` () =

    let expected = "00110101" + "000000011111" + "000000011111" + "00000001100" + "00000010111"
    let scanline = Array.create 7000 '0'
    group3Test scanline expected

[<Test>]
let ``Group 3, matching white sequence of 1`` () =

    let expected = "000111"
    let scanline = "1"
    group3Test scanline expected

[<Test>]
let ``Group 3, matching white sequence of 2`` () =

    let expected = "0111"
    let scanline = "11"
    group3Test scanline expected

[<Test>]
let ``Group 3, matching white sequence of 3`` () =

    let expected = "1000"
    let scanline = "111"
    group3Test scanline expected

[<Test>]
let ``Group 3, matching white sequence of 63`` () =

    let expected = "00110100"
    let scanline = Array.create 63 '1'
    group3Test scanline expected

[<Test>]
let ``Group 3, matching white sequence of 64`` () =

    let expected = "11011" + "00110101"
    let scanline = Array.create 64 '1'
    group3Test scanline expected

[<Test>]
let ``Group 3, matching white sequence of 65`` () =
 
    let expected = "11011" + "000111"
    let scanline = Array.create 65 '1'
    group3Test scanline expected

[<Test>]
let ``Group 3, matching white sequence of 700`` () =

    let expected = "01100111" + "01001011"
    let scanline = Array.create 700 '1'
    group3Test scanline expected

[<Test>]
let ``Group 3, matching white sequence of 7000`` () =

    let expected = "000000011111" + "000000011111" + "00000001100" + "0101000"
    let scanline = Array.create 7000 '1'
    group3Test scanline expected
