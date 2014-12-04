module BitonalDocs.Imaging

open System
open System.Drawing
open System.Drawing.Imaging
open System.Runtime.InteropServices

//-------------------------------------------------------------------------------------------------

let private pixelFormat = PixelFormat.Format32bppArgb

let private convertImageToArray (image : Bitmap) =

    let w = image.Width
    let h = image.Height

    let rect = Rectangle(Point.Empty, image.Size)
    let data = image.LockBits(rect, ImageLockMode.ReadOnly, pixelFormat)
    let byteCount = Math.Abs(data.Stride) * h
    let bytes = Array.zeroCreate<byte> byteCount
    Marshal.Copy(data.Scan0, bytes, 0, byteCount)
    image.UnlockBits(data)

    (w, h, bytes)

let private convertToMonochrome dither (w, h, bytes : byte[]) =

    let getPixelColor (x, y) =
        let i = 4 * (x + (y * w))
        let r = bytes.[i + 1]
        let g = bytes.[i + 2]
        let b = bytes.[i + 3]
        Dithering.Color(r, g, b)

    let computeValue (x, y) = dither getPixelColor (x, y)

    (w, h, computeValue)

let private convertTo1Bpp (w, h, getValue : (int * int) -> bool) =

    let stride = int (Math.Ceiling(float w / 8.0))

    let rec reduceBits offset acc = function
        | bits when bits = 0 -> acc
        | bits ->
            let x = (offset % w) + bits - 1
            let y = (offset / w)
            let pixel = match getValue (x, y) with true -> 0uy | false -> 1uy
            let value = acc ||| (pixel <<< (8 - bits))
            reduceBits offset value (bits - 1)

    let computeValue i =
        let offsetY = (i / stride) * w
        let offsetX = (i % stride) * 8
        let offset = offsetY + offsetX
        let bits = Math.Min(8, w - offsetX)
        reduceBits offset 0uy bits

    Array.Parallel.init (stride * h) computeValue

let createTiffImage width height resolution render =

    let resolution = float32 resolution
    let width = int (resolution * width)
    let height = int (resolution * height)

    use bitmap = new Bitmap(width, height, pixelFormat)
    bitmap.SetResolution(resolution, resolution)

    use graphics = Graphics.FromImage(bitmap)
    graphics.FillRectangle(Brushes.White, Rectangle(0, 0, width, height))
    render graphics

    bitmap
    |> convertImageToArray
    |> convertToMonochrome (Dithering.thresholdFixed 127uy)
    |> convertTo1Bpp
    |> Tiff.createImageFile (uint32 width) (uint32 height) (uint32 resolution)
    |> Tiff.serializeImageFile
