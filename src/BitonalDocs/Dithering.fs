module BitonalDocs.Dithering

//-------------------------------------------------------------------------------------------------

[<Struct>]
type Color =
    val R : byte
    val G : byte
    val B : byte
    new (r, g, b) = { R = r; G = g; B = b }

type Pixel =
    | Black
    | White

type DitheringType =
    | Threshold' of byte[,]
    | ErrorDiffusion' of ((int * int * int)[] * int)

//-------------------------------------------------------------------------------------------------

module Threshold =

    let fixed' threshold = Threshold' (array2D [ [ threshold ] ])

    let createMatrix levels thresholds =
        let mapping i = (((1 + i) * 256) / (1 + levels)) - 1
        let matrix = thresholds |> array2D |> Array2D.map (mapping >> byte)
        Threshold' matrix

    let dispersed8x8 =
        createMatrix (8 * 8)
            [ [ 00; 48; 12; 60; 03; 51; 15; 63 ]
              [ 32; 16; 44; 28; 35; 19; 47; 31 ]
              [ 08; 56; 04; 52; 11; 59; 07; 55 ]
              [ 40; 24; 36; 20; 43; 27; 39; 23 ]
              [ 02; 50; 14; 62; 01; 49; 13; 61 ]
              [ 34; 18; 46; 30; 33; 17; 45; 29 ]
              [ 10; 58; 06; 54; 09; 57; 05; 53 ]
              [ 42; 26; 38; 22; 41; 25; 37; 21 ] ]

    let dispersed4x4 =
        createMatrix (4 * 4)
            [ [ 00; 12; 03; 15 ]
              [ 08; 04; 11; 07 ]
              [ 02; 14; 01; 13 ]
              [ 10; 06; 09; 05 ] ]

    let dispersed2x2 =
        createMatrix (2 * 2)
            [ [ 00; 03 ]
              [ 02; 01 ] ]

    let clustered8x8 =
        createMatrix (4 * 8)
            [ [ 12; 05; 06; 13; 19; 26; 25; 18 ]
              [ 04; 00; 01; 07; 27; 31; 30; 24 ]
              [ 11; 03; 02; 08; 20; 28; 29; 23 ]
              [ 15; 10; 09; 14; 16; 21; 22; 17 ]
              [ 19; 26; 25; 18; 12; 05; 06; 13 ]
              [ 27; 31; 30; 24; 04; 00; 01; 07 ]
              [ 20; 28; 29; 23; 11; 03; 02; 08 ]
              [ 16; 21; 22; 17; 15; 10; 09; 14 ] ]

    let clustered6x6 =
        createMatrix (3 * 6)
            [ [ 08; 04; 05; 09; 13; 12 ]
              [ 03; 00; 01; 14; 17; 16 ]
              [ 07; 02; 06; 10; 15; 11 ]
              [ 09; 13; 12; 08; 04; 05 ]
              [ 14; 17; 16; 03; 00; 01 ]
              [ 10; 15; 11; 07; 02; 06 ] ]

    let clustered4x4 =
        createMatrix (2 * 4)
            [ [ 00; 01; 07; 06 ]
              [ 03; 02; 04; 05 ]
              [ 07; 06; 00; 01 ]
              [ 04; 05; 03; 02 ] ]

    let clustered2x2 =
        createMatrix (1 * 2)
            [ [ 00; 01 ]
              [ 01; 00 ] ]

//-------------------------------------------------------------------------------------------------

module ErrorDiffusion =

    let createFilter divisor coefficients = ErrorDiffusion' (coefficients, divisor)

    let basic =
        createFilter 01
            [| ( 1,  0, +1 ) |]

    let falseFloydSteinberg =
        createFilter 08
            [| ( 3,  0, +1 )
               ( 3, +1,  0 )
               ( 2, +1, +1 ) |]

    let floydSteinberg =
        createFilter 16
            [| ( 7,  0, +1 )
               ( 3, +1, -1 )
               ( 5, +1,  0 )
               ( 1, +1, +1 ) |]

    let jarvisJudiceNinke =
        createFilter 48
            [| ( 7,  0, +1 )
               ( 5,  0, +2 )
               ( 3, +1, -2 )
               ( 5, +1, -1 )
               ( 7, +1,  0 )
               ( 5, +1, +1 )
               ( 3, +1, +2 )
               ( 1, +2, -2 )
               ( 3, +2, -1 )
               ( 5, +2,  0 )
               ( 3, +2, +1 )
               ( 1, +2, +2 ) |]

    let stucki =
        createFilter 42
            [| ( 8,  0, +1 )
               ( 4,  0, +2 )
               ( 2, +1, -2 )
               ( 4, +1, -1 )
               ( 8, +1,  0 )
               ( 4, +1, +1 )
               ( 2, +1, +2 )
               ( 1, +2, -2 )
               ( 2, +2, -1 )
               ( 4, +2,  0 )
               ( 2, +2, +1 )
               ( 1, +2, +2 ) |]

    let burkes =
        createFilter 32
            [| ( 8,  0, +1 )
               ( 4,  0, +2 )
               ( 2, +1, -2 )
               ( 4, +1, -1 )
               ( 8, +1,  0 )
               ( 4, +1, +1 )
               ( 2, +1, +2 ) |]

    let sierra3Row =
        createFilter 32
            [| ( 5,  0, +1 )
               ( 3,  0, +2 )
               ( 2, +1, -2 )
               ( 4, +1, -1 )
               ( 5, +1,  0 )
               ( 4, +1, +1 )
               ( 2, +1, +2 )
               ( 2, +2, -1 )
               ( 3, +2,  0 )
               ( 2, +2, +1 ) |]

    let sierra2Row =
        createFilter 16
            [| ( 4,  0, +1 )
               ( 3,  0, +2 )
               ( 1, +1, -2 )
               ( 2, +1, -1 )
               ( 3, +1,  0 )
               ( 2, +1, +1 )
               ( 1, +1, +2 ) |]

    let sierraLite =
        createFilter 04
            [| ( 2,  0, +1 )
               ( 1, +1, -1 )
               ( 1, +1,  0 ) |]

    let atkinson =
        createFilter 08
            [| ( 1,  0, +1 )
               ( 1,  0, +2 )
               ( 1, +1, -1 )
               ( 1, +1,  0 )
               ( 1, +1, +1 )
               ( 1, +2,  0 ) |]

    let zhigangFan =
        createFilter 16
            [| ( 7,  0, +1 )
               ( 1, +1, -1 )
               ( 3, +1,  0 )
               ( 5, +1, +1 ) |]

    let shiauFan1 =
        createFilter 08
            [| ( 4,  0, +1 )
               ( 1, +1, -2 )
               ( 1, +1, -1 )
               ( 2, +1,  0 ) |]

    let shiauFan2 =
        createFilter 16
            [| ( 8,  0, +1 )
               ( 1, +1, -3 )
               ( 1, +1, -2 )
               ( 2, +1, -1 )
               ( 4, +1,  0 ) |]

//-------------------------------------------------------------------------------------------------

let private computeBrightness (image : Color[,]) row col =

    let color = image.[row, col]
    let r = int color.R * 2126
    let g = int color.G * 7152
    let b = int color.B * 0722
    let brightness = (r + g + b) / 10000
    byte brightness

let private ditherThreshold matrix image =

    let computePixel row col =
        let brightness = computeBrightness image row col
        let m = Array2D.length1 matrix
        let n = Array2D.length2 matrix
        let threshold = matrix.[row % m, col % n]
        match brightness > threshold with
        | false -> Black
        | true  -> White

    let rows = Array2D.length1 image
    let cols = Array2D.length2 image
    Array2D.init rows cols computePixel

let private ditherErrorDiffusion filter image =

    let rows = Array2D.length1 image
    let cols = Array2D.length2 image
    let pixels = Array2D.zeroCreate<Pixel> rows cols
    let errors = Array2D.zeroCreate<sbyte> rows cols

    let computePixelAndError row col =
        let brightness = computeBrightness image row col
        let value = int brightness + int errors.[row, col]
        match value > 127 with
        | false -> Black, sbyte (value)
        | true  -> White, sbyte (value - 255)

    let writePixel pixel row col =
        pixels.[row, col] <- pixel

    let writeError error row col =
        let coefficients, divisor = filter : (int * int * int)[] * int
        for coefficient, row', col' in coefficients do
            let row = row + row'
            let col = col + col'
            if (0 <= row && row < rows) && (0 <= col && col < cols) then
                let error = (int error * coefficient) / divisor
                errors.[row, col] <- errors.[row, col] + sbyte error

    for row = 0 to rows - 1 do
        for col = 0 to cols - 1 do
            let pixel, error = computePixelAndError row col
            writePixel pixel row col
            writeError error row col

    pixels

let dither = function
    | Threshold'
        matrix -> matrix |> ditherThreshold
    | ErrorDiffusion'
        filter -> filter |> ditherErrorDiffusion
