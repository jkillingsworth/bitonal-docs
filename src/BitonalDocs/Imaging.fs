module BitonalDocs.Imaging

open System
open System.Drawing
open System.Drawing.Imaging
open System.Runtime.InteropServices

//-------------------------------------------------------------------------------------------------

let private pixelFormat = PixelFormat.Format32bppArgb

let private convertTo1BppByteArray (bitmap : Bitmap) =

    let width = bitmap.Width
    let height = bitmap.Height

    let bitmapRect = Rectangle(0, 0, width, height)
    let bitmapData = bitmap.LockBits(bitmapRect, ImageLockMode.ReadOnly, pixelFormat)
    let bitmapByteCount = Math.Abs(bitmapData.Stride) * height
    let bitmapBytes = Array.zeroCreate<byte> bitmapByteCount
    Marshal.Copy(bitmapData.Scan0, bitmapBytes, 0, bitmapByteCount)
    bitmap.UnlockBits(bitmapData)

    let stride = int (Math.Ceiling(float width / 8.0))

    let inline getColor i =
        let i = i * 4
        let r = int bitmapBytes.[i + 1]
        let g = int bitmapBytes.[i + 2]
        let b = int bitmapBytes.[i + 3]
        let brightness = (r + r + r + b + g + g + g + g) >>> 3
        match brightness > 127 with true -> 0uy | false -> 1uy

    let rec reduceBits offset acc = function
        | bits when bits = 0 -> acc
        | bits ->
            let index = offset + bits - 1
            let pixel = getColor index
            let value = acc ||| (pixel <<< (8 - bits))
            reduceBits offset value (bits - 1)

    let inline getValue i =
        let offsetY = (i / stride) * width
        let offsetX = (i % stride) * 8
        let offset = offsetY + offsetX
        let bits = Math.Min(8, width - offsetX)
        reduceBits offset 0uy bits

    Array.Parallel.init (stride * height) getValue

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
    |> convertTo1BppByteArray
    |> Tiff.createImageFile (uint32 width) (uint32 height) (uint32 resolution)
    |> Tiff.serializeImageFile
