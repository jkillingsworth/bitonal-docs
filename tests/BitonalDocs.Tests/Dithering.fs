module BitonalDocs.Tests.Dithering

open NUnit.Framework
open FsUnit
open BitonalDocs.Imaging
open BitonalDocs.Dithering

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Threshold, custom matrix should have correct values`` () =

    let matrix = Threshold.createMatrix (1 * 3) [ [ 00; 01; 02 ] ]

    match matrix with
    | Threshold' matrix
        ->
        matrix.[0, 0] |> should equal 063uy
        matrix.[0, 1] |> should equal 127uy
        matrix.[0, 2] |> should equal 191uy
    | _
        -> Assert.Fail()

[<Test>]
let ``Threshold, dithering works correctly`` () =

    let expected =
        array2D
            [ [ Black; Black; Black; Black; Black; Black ]
              [ White; Black; Black; White; Black; Black ]
              [ White; White; Black; White; White; Black ]
              [ White; White; White; White; White; White ] ]

    let shades =
        array2D
            [ [ 000uy; 000uy; 000uy; 063uy; 063uy; 063uy ]
              [ 064uy; 064uy; 064uy; 127uy; 127uy; 127uy ]
              [ 128uy; 128uy; 128uy; 191uy; 191uy; 191uy ]
              [ 192uy; 192uy; 192uy; 255uy; 255uy; 255uy ] ]

    let colors = shades |> Array2D.map (fun shade -> { R = shade; G = shade; B = shade})
    let matrix = Threshold.createMatrix (1 * 3) [ [ 00; 01; 02 ] ]
    let pixels = colors |> dither matrix

    pixels.[0, 0] |> should equal expected.[0, 0]
    pixels.[0, 1] |> should equal expected.[0, 1]
    pixels.[0, 2] |> should equal expected.[0, 2]
    pixels.[0, 3] |> should equal expected.[0, 3]
    pixels.[0, 4] |> should equal expected.[0, 4]
    pixels.[0, 5] |> should equal expected.[0, 5]

    pixels.[1, 0] |> should equal expected.[1, 0]
    pixels.[1, 1] |> should equal expected.[1, 1]
    pixels.[1, 2] |> should equal expected.[1, 2]
    pixels.[1, 3] |> should equal expected.[1, 3]
    pixels.[1, 4] |> should equal expected.[1, 4]
    pixels.[1, 5] |> should equal expected.[1, 5]

    pixels.[2, 0] |> should equal expected.[2, 0]
    pixels.[2, 1] |> should equal expected.[2, 1]
    pixels.[2, 2] |> should equal expected.[2, 2]
    pixels.[2, 3] |> should equal expected.[2, 3]
    pixels.[2, 4] |> should equal expected.[2, 4]
    pixels.[2, 5] |> should equal expected.[2, 5]

    pixels.[3, 0] |> should equal expected.[3, 0]
    pixels.[3, 1] |> should equal expected.[3, 1]
    pixels.[3, 2] |> should equal expected.[3, 2]
    pixels.[3, 3] |> should equal expected.[3, 3]
    pixels.[3, 4] |> should equal expected.[3, 4]
    pixels.[3, 5] |> should equal expected.[3, 5]

//-------------------------------------------------------------------------------------------------

[<Test>]
let ``Error diffusion, custom filter should have correct values`` () =

    let filter = ErrorDiffusion.createFilter 16 [| ( 8,  0, +1 ); ( 4, +1, -1 ); ( 2, +1,  0 ); ( 1, +1, +1 ) |]

    match filter with
    | ErrorDiffusion' filter
        ->
        let coefficients, divisor = filter
        divisor |> should equal 16
        coefficients.[0] |> should equal ( 8,  0, +1 )
        coefficients.[1] |> should equal ( 4, +1, -1 )
        coefficients.[2] |> should equal ( 2, +1,  0 )
        coefficients.[3] |> should equal ( 1, +1, +1 )
    | _
        -> Assert.Fail()

[<Test>]
let ``Error diffusion, dithering works correctly, scenario 1`` () =

    let expected =
        array2D
            [ [ Black; Black; White ]
              [ White; Black; White ] ]

    let shades =
        array2D
            [ [ 000uy; 016uy; 120uy ]
              [ 124uy; 219uy; 079uy ] ]

    let colors = shades |> Array2D.map (fun shade -> { R = shade; G = shade; B = shade})
    let filter = ErrorDiffusion.createFilter 16 [| ( 8,  0, +1 ); ( 4, +1, -1 ); ( 2, +1,  0 ); ( 1, +1, +1 ) |]
    let pixels = colors |> dither filter

    pixels.[0, 0] |> should equal expected.[0, 0]
    pixels.[0, 1] |> should equal expected.[0, 1]
    pixels.[0, 2] |> should equal expected.[0, 2]
    pixels.[1, 0] |> should equal expected.[1, 0]
    pixels.[1, 1] |> should equal expected.[1, 1]
    pixels.[1, 2] |> should equal expected.[1, 2]

[<Test>]
let ``Error diffusion, dithering works correctly, scenario 2`` () =

    let expected =
        array2D
            [ [ White; White; Black ]
              [ Black; White; Black ] ]

    let shades =
        array2D
            [ [ 255uy; 239uy; 135uy ]
              [ 131uy; 036uy; 176uy ] ]

    let colors = shades |> Array2D.map (fun shade -> { R = shade; G = shade; B = shade})
    let filter = ErrorDiffusion.createFilter 16 [| ( 8,  0, +1 ); ( 4, +1, -1 ); ( 2, +1,  0 ); ( 1, +1, +1 ) |]
    let pixels = colors |> dither filter

    pixels.[0, 0] |> should equal expected.[0, 0]
    pixels.[0, 1] |> should equal expected.[0, 1]
    pixels.[0, 2] |> should equal expected.[0, 2]
    pixels.[1, 0] |> should equal expected.[1, 0]
    pixels.[1, 1] |> should equal expected.[1, 1]
    pixels.[1, 2] |> should equal expected.[1, 2]

[<Test>]
let ``Error diffusion, Floyd-Steinberg filter renders checkerboard pattern for solid gray 127`` () =

    let shade = 127uy

    let expectedInitializer row col = if (row % 2) = (col % 2) then Black else White
    let expected = Array2D.init 512 512 expectedInitializer
    let colors = Array2D.create 512 512 { R = shade; G = shade; B = shade}
    let pixels = colors |> dither ErrorDiffusion.floydSteinberg

    pixels.[0, 0] |> should equal Black
    pixels.[0, 1] |> should equal White
    pixels.[1, 0] |> should equal White
    pixels.[1, 1] |> should equal Black

    pixels |> should equal expected

[<Test>]
let ``Error diffusion, Floyd-Steinberg filter renders checkerboard pattern for solid gray 128`` () =

    let shade = 128uy

    let expectedInitializer row col = if (row % 2) = (col % 2) then White else Black
    let expected = Array2D.init 512 512 expectedInitializer
    let colors = Array2D.create 512 512 { R = shade; G = shade; B = shade}
    let pixels = colors |> dither ErrorDiffusion.floydSteinberg

    pixels.[0, 0] |> should equal White
    pixels.[0, 1] |> should equal Black
    pixels.[1, 0] |> should equal Black
    pixels.[1, 1] |> should equal White

    pixels |> should equal expected
