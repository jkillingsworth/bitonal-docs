module BitonalDocs.Dithering

//-------------------------------------------------------------------------------------------------

[<Struct>]
type Color =

    val R : byte
    val G : byte
    val B : byte

    new (r, g, b) = { R = r; G = g; B = b }

type ErrorDiffusionElement = { X : int; Y : int; Coefficient : int; Divisor : int }

type DitheringType =
    | ThresholdFixed of byte
    | ThresholdOrdered of byte[,]
    | ErrorDiffusion' of ErrorDiffusionElement[]

//-------------------------------------------------------------------------------------------------

module Threshold =

    let fixed' level = ThresholdFixed level

    let createMatrix levels thresholds =
        let array = array2D thresholds
        let mapping i = byte (((1 + i) * 256) / (1 + levels))
        let matrix = Array2D.map mapping array
        ThresholdOrdered matrix

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

    let createFilter elements =
        ErrorDiffusion' elements

    let basic =
        createFilter
            [| { X = +1; Y =  0; Coefficient = 1; Divisor =  1 } |]

    let falseFloydSteinberg =
        createFilter
            [| { X = +1; Y =  0; Coefficient = 3; Divisor =  8 }
               { X =  0; Y = +1; Coefficient = 3; Divisor =  8 }
               { X = +1; Y = +1; Coefficient = 2; Divisor =  8 } |]

    let floydSteinberg =
        createFilter
            [| { X = +1; Y =  0; Coefficient = 7; Divisor = 16 }
               { X = -1; Y = +1; Coefficient = 3; Divisor = 16 }
               { X =  0; Y = +1; Coefficient = 5; Divisor = 16 }
               { X = +1; Y = +1; Coefficient = 1; Divisor = 16 } |]

    let jarvisJudiceNinke =
        createFilter
            [| { X = +1; Y =  0; Coefficient = 7; Divisor = 48 }
               { X = +2; Y =  0; Coefficient = 5; Divisor = 48 }
               { X = -2; Y = +1; Coefficient = 3; Divisor = 48 }
               { X = -1; Y = +1; Coefficient = 5; Divisor = 48 }
               { X =  0; Y = +1; Coefficient = 7; Divisor = 48 }
               { X = +1; Y = +1; Coefficient = 5; Divisor = 48 }
               { X = +2; Y = +1; Coefficient = 3; Divisor = 48 }
               { X = -2; Y = +2; Coefficient = 1; Divisor = 48 }
               { X = -1; Y = +2; Coefficient = 3; Divisor = 48 }
               { X =  0; Y = +2; Coefficient = 5; Divisor = 48 }
               { X = +1; Y = +2; Coefficient = 3; Divisor = 48 }
               { X = +2; Y = +2; Coefficient = 1; Divisor = 48 } |]

    let stucki =
        createFilter
            [| { X = +1; Y =  0; Coefficient = 8; Divisor = 42 }
               { X = +2; Y =  0; Coefficient = 4; Divisor = 42 }
               { X = -2; Y = +1; Coefficient = 2; Divisor = 42 }
               { X = -1; Y = +1; Coefficient = 4; Divisor = 42 }
               { X =  0; Y = +1; Coefficient = 8; Divisor = 42 }
               { X = +1; Y = +1; Coefficient = 4; Divisor = 42 }
               { X = +2; Y = +1; Coefficient = 2; Divisor = 42 }
               { X = -2; Y = +2; Coefficient = 1; Divisor = 42 }
               { X = -1; Y = +2; Coefficient = 2; Divisor = 42 }
               { X =  0; Y = +2; Coefficient = 4; Divisor = 42 }
               { X = +1; Y = +2; Coefficient = 2; Divisor = 42 }
               { X = +2; Y = +2; Coefficient = 1; Divisor = 42 } |]

    let burkes =
        createFilter
            [| { X = +1; Y =  0; Coefficient = 8; Divisor = 32 }
               { X = +2; Y =  0; Coefficient = 4; Divisor = 32 }
               { X = -2; Y = +1; Coefficient = 2; Divisor = 32 }
               { X = -1; Y = +1; Coefficient = 4; Divisor = 32 }
               { X =  0; Y = +1; Coefficient = 8; Divisor = 32 }
               { X = +1; Y = +1; Coefficient = 4; Divisor = 32 }
               { X = +2; Y = +1; Coefficient = 2; Divisor = 32 } |]

    let sierra3Row =
        createFilter
            [| { X = +1; Y =  0; Coefficient = 5; Divisor = 32 }
               { X = +2; Y =  0; Coefficient = 3; Divisor = 32 }
               { X = -2; Y = +1; Coefficient = 2; Divisor = 32 }
               { X = -1; Y = +1; Coefficient = 4; Divisor = 32 }
               { X =  0; Y = +1; Coefficient = 5; Divisor = 32 }
               { X = +1; Y = +1; Coefficient = 4; Divisor = 32 }
               { X = +2; Y = +1; Coefficient = 2; Divisor = 32 }
               { X = -1; Y = +2; Coefficient = 2; Divisor = 32 }
               { X =  0; Y = +2; Coefficient = 3; Divisor = 32 }
               { X = +1; Y = +2; Coefficient = 2; Divisor = 32 } |]

    let sierra2Row =
        createFilter
            [| { X = +1; Y =  0; Coefficient = 4; Divisor = 16 }
               { X = +2; Y =  0; Coefficient = 3; Divisor = 16 }
               { X = -2; Y = +1; Coefficient = 1; Divisor = 16 }
               { X = -1; Y = +1; Coefficient = 2; Divisor = 16 }
               { X =  0; Y = +1; Coefficient = 3; Divisor = 16 }
               { X = +1; Y = +1; Coefficient = 2; Divisor = 16 }
               { X = +2; Y = +1; Coefficient = 1; Divisor = 16 } |]

    let sierraLite =
        createFilter
            [| { X = +1; Y =  0; Coefficient = 2; Divisor =  4 }
               { X = -1; Y = +1; Coefficient = 1; Divisor =  4 }
               { X =  0; Y = +1; Coefficient = 1; Divisor =  4 } |]

    let atkinson =
        createFilter
            [| { X = +1; Y =  0; Coefficient = 1; Divisor =  8 }
               { X = +2; Y =  0; Coefficient = 1; Divisor =  8 }
               { X = -1; Y = +1; Coefficient = 1; Divisor =  8 }
               { X =  0; Y = +1; Coefficient = 1; Divisor =  8 }
               { X = +1; Y = +1; Coefficient = 1; Divisor =  8 }
               { X =  0; Y = +2; Coefficient = 1; Divisor =  8 } |]

    let zhigangFan =
        createFilter
            [| { X = +1; Y =  0; Coefficient = 7; Divisor = 16 }
               { X = -1; Y = +1; Coefficient = 1; Divisor = 16 }
               { X =  0; Y = +1; Coefficient = 3; Divisor = 16 }
               { X = +1; Y = +1; Coefficient = 5; Divisor = 16 } |]

    let shiauFan1 =
        createFilter
            [| { X = +1; Y =  0; Coefficient = 4; Divisor =  8 }
               { X = -2; Y = +1; Coefficient = 1; Divisor =  8 }
               { X = -1; Y = +1; Coefficient = 1; Divisor =  8 }
               { X =  0; Y = +1; Coefficient = 2; Divisor =  8 } |]

    let shiauFan2 =
        createFilter
            [| { X = +1; Y =  0; Coefficient = 8; Divisor = 16 }
               { X = -3; Y = +1; Coefficient = 1; Divisor = 16 }
               { X = -2; Y = +1; Coefficient = 1; Divisor = 16 }
               { X = -1; Y = +1; Coefficient = 2; Divisor = 16 }
               { X =  0; Y = +1; Coefficient = 4; Divisor = 16 } |]

//-------------------------------------------------------------------------------------------------

let private computeBrightness (image : Color[,]) x y =

    let color = image.[x, y]
    let r = int color.R * 2126
    let g = int color.G * 7152
    let b = int color.B * 0722
    let brightness = (r + g + b) / 10000
    byte brightness

let private threshold matrix image =

    let computeThreshold matrix image x y =
        let m = Array2D.length1 matrix
        let n = Array2D.length2 matrix
        let brightness = computeBrightness image x y
        let threshold = matrix.[x % m, y % n]
        brightness > threshold

    let w = Array2D.length1 image
    let h = Array2D.length2 image
    Array2D.init w h (computeThreshold matrix image)

let private errorDiffusion filter image =

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
        for element in (filter : ErrorDiffusionElement[]) do
            let x = x + element.X
            let y = y + element.Y
            if (0 <= x && x < w) && (0 <= y && y < h) then
                let error = (int error * element.Coefficient) / element.Divisor
                errors.[x, y] <- errors.[x, y] + sbyte error

    for y = 0 to h - 1 do
        for x = 0 to w - 1 do
            let pixel, error = computePixelAndError x y
            writePixel pixel x y
            writeError error x y

    pixels

let dither pattern image =

    let rec computeDither = function
        | ThresholdFixed(value)
            -> [[ value ]]
            |> array2D
            |> ThresholdOrdered
            |> computeDither
        | ThresholdOrdered(matrix)
            -> matrix
            |> threshold
        | ErrorDiffusion'(filter)
            -> filter
            |> errorDiffusion

    computeDither pattern image
