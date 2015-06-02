module BitonalDocs.Rendering

open System
open System.Drawing
open System.Drawing.Imaging
open System.Runtime.InteropServices
open BitonalDocs.Imaging
open BitonalDocs.Compression
open BitonalDocs.Dithering
open BitonalDocs.Tiff

//-------------------------------------------------------------------------------------------------

let private bitmapPixelFormat = PixelFormat.Format32bppArgb

let private convertBitmapToColors (image : Bitmap) =

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

let private compression = function
    | None -> Tiff.Compression.None
    | Group3OneDimensional -> Tiff.Compression.Group3OneDimensional
    | PackBits -> Tiff.Compression.PackBits

let createTiffImage w h resolution render compressionType ditheringType =

    let resolution = single resolution
    let w = int (ceil (resolution * w))
    let h = int (ceil (resolution * h))

    use bitmap = new Bitmap(w, h, bitmapPixelFormat)
    bitmap.SetResolution(resolution, resolution)

    use graphics = Graphics.FromImage(bitmap)
    render graphics

    bitmap
    |> convertBitmapToColors
    |> dither ditheringType
    |> compress compressionType
    |> Tiff.createImageFile (uint32 w) (uint32 h) (uint32 resolution) (compression compressionType)
    |> Tiff.serializeImageFile
