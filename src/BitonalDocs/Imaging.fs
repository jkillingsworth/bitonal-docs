module BitonalDocs.Imaging

open System
open System.Drawing
open System.Drawing.Imaging
open System.Runtime.InteropServices

//-------------------------------------------------------------------------------------------------

type Color =
    { R : byte
      G : byte
      B : byte }

type Pixel =
    | Black
    | White

type Bit =
    | Bit0
    | Bit1

//-------------------------------------------------------------------------------------------------

let bitmapPixelFormat = PixelFormat.Format32bppArgb

let convertBitmapToColors (image : Bitmap) =

    let rows = image.Height
    let cols = image.Width
    let rect = Rectangle(Point.Empty, image.Size)
    let data = image.LockBits(rect, ImageLockMode.ReadOnly, bitmapPixelFormat);
    let byteCount = Math.Abs(data.Stride) * rows
    let bytes = Array.zeroCreate<byte> byteCount
    Marshal.Copy(data.Scan0, bytes, 0, byteCount)
    image.UnlockBits(data)

    let computeValue row col =
        let i = ((row * cols) + col) * 4
        { R = bytes.[i + 2]
          G = bytes.[i + 1]
          B = bytes.[i + 0] }

    Array2D.init rows cols computeValue

let convertPixelArrayToSeqPixels image =

    let rows = Array2D.length1 image
    let cols = Array2D.length2 image
    let computeValue row col = image.[row, col]
    let computeArray row = Array.init cols (computeValue row)
    Seq.init rows computeArray

let convertBitsTo1BppBytes bits =

    let length = Array.length bits
    let stride = int (ceil (double length / 8.0))

    let rec reduceBits offset acc = function
        | append when append = 0 -> acc
        | append ->
            let index = offset + append - 1
            let pixel = match bits.[index] with Bit0 -> 0uy | Bit1 -> 1uy
            let value = acc ||| (pixel <<< (8 - append))
            reduceBits offset value (append - 1)

    let computeValue i =
        let offset = i * 8
        let append = Math.Min(8, length - offset)
        reduceBits offset 0uy append

    Array.init stride computeValue

let convertPixelToBit = function
    | Black -> Bit1
    | White -> Bit0
