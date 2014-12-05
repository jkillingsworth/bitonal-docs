module BitonalDocs.Dithering

//-------------------------------------------------------------------------------------------------

[<Struct>]
type Color =

    val R : byte
    val G : byte
    val B : byte

    new (r, g, b) = { R = r; G = g; B = b }

//-------------------------------------------------------------------------------------------------

module Matrix =

    let createMatrix levels thresholds =

        let array = array2D thresholds
        let mapping i = byte (((1 + i) * 256) / (1 + levels))
        Array2D.map mapping array

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

let private computeBrightness (image : Color[,]) x y =

    let color = image.[x, y]
    let r = int color.R * 2
    let g = int color.G * 5
    let b = int color.B * 1
    let brightness = (r + g + b) / 8

    byte brightness

let private computeThresholdFixed threshold image x y =

    let brightness = computeBrightness image x y
    brightness > threshold

let private computeThresholdOrdered matrix image x y =

    let m = Array2D.length1 matrix
    let n = Array2D.length2 matrix
    let threshold = matrix.[x % m, y % n]
    computeThresholdFixed threshold image x y

let thresholdFixed threshold image =

    let w = Array2D.length1 image
    let h = Array2D.length2 image
    Array2D.init w h (computeThresholdFixed threshold image)

let thresholdOrdered matrix image =

    let w = Array2D.length1 image
    let h = Array2D.length2 image
    Array2D.init w h (computeThresholdOrdered matrix image)
