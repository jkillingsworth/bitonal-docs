module BitonalDocs.Dithering

//-------------------------------------------------------------------------------------------------

[<Struct>]
type Color =

    val R : byte
    val G : byte
    val B : byte

    new (r, g, b) = { R = r; G = g; B = b }

type DitheringType =
    | Threshold' of byte[,]
    | ErrorDiffusion' of ((int * int * int)[] * int)

//-------------------------------------------------------------------------------------------------

module Threshold =

    let fixed' level = Threshold' (array2D [[ level ]])

    let createMatrix levels thresholds =
        let array = array2D thresholds
        let mapping i = byte (((1 + i) * 256) / (1 + levels))
        let matrix = Array2D.map mapping array
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
        createMatrix (8 * 4)
            [ [ 12; 05; 06; 13; 19; 26; 25; 18 ]
              [ 04; 00; 01; 07; 27; 31; 30; 24 ]
              [ 11; 03; 02; 08; 20; 28; 29; 23 ]
              [ 15; 10; 09; 14; 16; 21; 22; 17 ]
              [ 19; 26; 25; 18; 12; 05; 06; 13 ]
              [ 27; 31; 30; 24; 04; 00; 01; 07 ]
              [ 20; 28; 29; 23; 11; 03; 02; 08 ]
              [ 16; 21; 22; 17; 15; 10; 09; 14 ] ]

    let clustered6x6 =
        createMatrix (6 * 3)
            [ [ 08; 04; 05; 09; 13; 12 ]
              [ 03; 00; 01; 14; 17; 16 ]
              [ 07; 02; 06; 10; 15; 11 ]
              [ 09; 13; 12; 08; 04; 05 ]
              [ 14; 17; 16; 03; 00; 01 ]
              [ 10; 15; 11; 07; 02; 06 ] ]

    let clustered4x4 =
        createMatrix (4 * 2)
            [ [ 00; 01; 07; 06 ]
              [ 03; 02; 04; 05 ]
              [ 07; 06; 00; 01 ]
              [ 04; 05; 03; 02 ] ]

    let clustered2x2 =
        createMatrix (2 * 1)
            [ [ 00; 01 ]
              [ 01; 00 ] ]

//-------------------------------------------------------------------------------------------------

module ErrorDiffusion =

    let createFilter divisor coefficients = ErrorDiffusion' (coefficients, divisor)

    let basic =
        createFilter 01
            [| ( 1, +1,  0 ) |]

    let falseFloydSteinberg =
        createFilter 08
            [| ( 3, +1,  0 )
               ( 3,  0, +1 )
               ( 2, +1, +1 ) |]

    let floydSteinberg =
        createFilter 16
            [| ( 7, +1,  0 )
               ( 3, -1, +1 )
               ( 5,  0, +1 )
               ( 1, +1, +1 ) |]

    let jarvisJudiceNinke =
        createFilter 48
            [| ( 7, +1,  0 )
               ( 5, +2,  0 )
               ( 3, -2, +1 )
               ( 5, -1, +1 )
               ( 7,  0, +1 )
               ( 5, +1, +1 )
               ( 3, +2, +1 )
               ( 1, -2, +2 )
               ( 3, -1, +2 )
               ( 5,  0, +2 )
               ( 3, +1, +2 )
               ( 1, +2, +2 ) |]

    let stucki =
        createFilter 42
            [| ( 8, +1,  0 )
               ( 4, +2,  0 )
               ( 2, -2, +1 )
               ( 4, -1, +1 )
               ( 8,  0, +1 )
               ( 4, +1, +1 )
               ( 2, +2, +1 )
               ( 1, -2, +2 )
               ( 2, -1, +2 )
               ( 4,  0, +2 )
               ( 2, +1, +2 )
               ( 1, +2, +2 ) |]

    let burkes =
        createFilter 32
            [| ( 8, +1,  0 )
               ( 4, +2,  0 )
               ( 2, -2, +1 )
               ( 4, -1, +1 )
               ( 8,  0, +1 )
               ( 4, +1, +1 )
               ( 2, +2, +1 ) |]

    let sierra3Row =
        createFilter 32
            [| ( 5, +1,  0 )
               ( 3, +2,  0 )
               ( 2, -2, +1 )
               ( 4, -1, +1 )
               ( 5,  0, +1 )
               ( 4, +1, +1 )
               ( 2, +2, +1 )
               ( 2, -1, +2 )
               ( 3,  0, +2 )
               ( 2, +1, +2 ) |]

    let sierra2Row =
        createFilter 16
            [| ( 4, +1,  0 )
               ( 3, +2,  0 )
               ( 1, -2, +1 )
               ( 2, -1, +1 )
               ( 3,  0, +1 )
               ( 2, +1, +1 )
               ( 1, +2, +1 ) |]

    let sierraLite =
        createFilter 04
            [| ( 2, +1,  0 )
               ( 1, -1, +1 )
               ( 1,  0, +1 ) |]

    let atkinson =
        createFilter 08
            [| ( 1, +1,  0 )
               ( 1, +2,  0 )
               ( 1, -1, +1 )
               ( 1,  0, +1 )
               ( 1, +1, +1 )
               ( 1,  0, +2 ) |]

    let zhigangFan =
        createFilter 16
            [| ( 7, +1,  0 )
               ( 1, -1, +1 )
               ( 3,  0, +1 )
               ( 5, +1, +1 ) |]

    let shiauFan1 =
        createFilter 08
            [| ( 4, +1,  0 )
               ( 1, -2, +1 )
               ( 1, -1, +1 )
               ( 2,  0, +1 ) |]

    let shiauFan2 =
        createFilter 16
            [| ( 8, +1,  0 )
               ( 1, -3, +1 )
               ( 1, -2, +1 )
               ( 2, -1, +1 )
               ( 4,  0, +1 ) |]

//-------------------------------------------------------------------------------------------------

let private computeBrightness (image : Color[,]) x y =

    let color = image.[x, y]
    let r = int color.R * 2126
    let g = int color.G * 7152
    let b = int color.B * 0722
    let brightness = (r + g + b) / 10000
    byte brightness

let private ditherThreshold matrix image =

    let computeThreshold matrix image x y =
        let m = Array2D.length1 matrix
        let n = Array2D.length2 matrix
        let brightness = computeBrightness image x y
        let threshold = matrix.[x % m, y % n]
        brightness > threshold

    let w = Array2D.length1 image
    let h = Array2D.length2 image
    Array2D.init w h (computeThreshold matrix image)

let private ditherErrorDiffusion filter image =

    let w = Array2D.length1 image
    let h = Array2D.length2 image
    let pixels = Array2D.zeroCreate<bool> w h
    let errors = Array2D.zeroCreate<sbyte> w h

    let computePixelAndError x y =
        let brightness = computeBrightness image x y
        let value = int brightness + int errors.[x, y]
        let pixel = value > 127
        let error = sbyte (match pixel with true -> (value - 255) | false -> value)
        pixel, error

    let writePixel pixel x y =
        pixels.[x, y] <- pixel

    let writeError error x y =
        let coefficients, divisor = filter : (int * int * int)[] * int
        for coefficient, x', y' in coefficients do
            let x = x + x'
            let y = y + y'
            if (0 <= x && x < w) && (0 <= y && y < h) then
                let error = (int error * coefficient) / divisor
                errors.[x, y] <- errors.[x, y] + sbyte error

    for y = 0 to h - 1 do
        for x = 0 to w - 1 do
            let pixel, error = computePixelAndError x y
            writePixel pixel x y
            writeError error x y

    pixels

let dither = function
    | Threshold'(matrix)
        -> matrix
        |> ditherThreshold
    | ErrorDiffusion'(filter)
        -> filter
        |> ditherErrorDiffusion
