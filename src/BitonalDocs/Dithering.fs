module Dithering

//-------------------------------------------------------------------------------------------------

[<Struct>]
type Color =

    val R : byte
    val G : byte
    val B : byte

    new (r, g, b) = { R = r; G = g; B = b }

type GetPixelColor = (int * int) -> Color

//-------------------------------------------------------------------------------------------------

let threshold value (getPixelColor : GetPixelColor) (x, y) =

    let color = getPixelColor (x, y)
    let r = int color.R
    let g = int color.G
    let b = int color.B
    let brightness = (r + r + r + b + g + g + g + g) >>> 3

    (byte brightness > value)
