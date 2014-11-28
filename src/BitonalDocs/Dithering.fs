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
    let r = int color.R * 2
    let g = int color.G * 5
    let b = int color.B * 1
    let brightness = (r + g + b) / 8

    (byte brightness > value)
