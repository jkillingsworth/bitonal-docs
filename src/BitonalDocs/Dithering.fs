module BitonalDocs.Dithering

//-------------------------------------------------------------------------------------------------

[<Struct>]
type Color =

    val R : byte
    val G : byte
    val B : byte

    new (r, g, b) = { R = r; G = g; B = b }

type GetPixelColor = (int * int) -> Color

//-------------------------------------------------------------------------------------------------

module Matrix =

    let bayer8x8 =
        [ [ 00uy; 48uy; 12uy; 60uy; 03uy; 51uy; 15uy; 63uy ]
          [ 32uy; 16uy; 44uy; 28uy; 35uy; 19uy; 47uy; 31uy ]
          [ 08uy; 56uy; 04uy; 52uy; 11uy; 59uy; 07uy; 55uy ]
          [ 40uy; 24uy; 36uy; 20uy; 43uy; 27uy; 39uy; 23uy ]
          [ 02uy; 50uy; 14uy; 62uy; 01uy; 49uy; 13uy; 61uy ]
          [ 34uy; 18uy; 46uy; 30uy; 33uy; 17uy; 45uy; 29uy ]
          [ 10uy; 58uy; 06uy; 54uy; 09uy; 57uy; 05uy; 53uy ]
          [ 42uy; 26uy; 38uy; 22uy; 41uy; 25uy; 37uy; 21uy ] ]

    let bayer4x4 =
        [ [ 00uy; 12uy; 03uy; 15uy ]
          [ 08uy; 04uy; 11uy; 07uy ]
          [ 02uy; 14uy; 01uy; 13uy ]
          [ 10uy; 06uy; 09uy; 05uy ] ]

    let bayer2x2 =
        [ [ 00uy; 03uy ]
          [ 02uy; 01uy ] ]

//-------------------------------------------------------------------------------------------------

let private computeBrightness (getPixelColor : GetPixelColor) (x, y) =

    let color = getPixelColor (x, y)
    let r = int color.R * 2
    let g = int color.G * 5
    let b = int color.B * 1
    let brightness = (r + g + b) / 8

    byte brightness

let threshold value getPixelColor (x, y) = computeBrightness getPixelColor (x, y) > value

let ordered (matrix : seq<#seq<byte>>) getPixelColor (x, y) =

    let brightness = computeBrightness getPixelColor (x, y)
    let shades = int System.Byte.MaxValue + 1
    let matrix = array2D matrix
    let m = Array2D.length1 matrix
    let n = Array2D.length2 matrix
    let i = matrix.[x % m, y % n]
    let value = ((1 + int i) * shades) / (1 + (m * n))
    let value = byte value

    brightness > value
