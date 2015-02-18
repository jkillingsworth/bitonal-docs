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

let convertPixelsTo1BppScanlines (image : Pixel[,]) =

    let rows = Array2D.length1 image
    let cols = Array2D.length2 image
    let stride = int (ceil (double cols / 8.0))

    let rec reduceBits row offset acc = function
        | bits when bits = 0 -> acc
        | bits ->
            let col = (offset % cols) + bits - 1
            let pixel = match image.[row, col] with Black -> 1uy | White -> 0uy
            let value = acc ||| (pixel <<< (8 - bits))
            reduceBits row offset value (bits - 1)

    let computeValue row x =
        let offset = x * 8
        let bits = Math.Min(8, cols - offset)
        reduceBits row offset 0uy bits

    Array.init rows (fun row -> Array.init stride (computeValue row))

let convertScanlinesToSingleStrip (image : byte[][]) =

    image |> Array.concat
