module BitonalDocs.Rendering

open System
open System.Drawing
open BitonalDocs.Imaging
open BitonalDocs.Dithering

//-------------------------------------------------------------------------------------------------

let createTiffImage w h resolution render =

    let resolution = single resolution
    let w = int (ceil (resolution * w))
    let h = int (ceil (resolution * h))

    use bitmap = new Bitmap(w, h, bitmapPixelFormat)
    bitmap.SetResolution(resolution, resolution)

    use graphics = Graphics.FromImage(bitmap)
    render graphics

    bitmap
    |> convertBitmapToColors
    |> dither (Threshold.fixed' 127uy)
    |> convertPixelsTo1Bpp
    |> Tiff.createImageFile (uint32 w) (uint32 h) (uint32 resolution)
    |> Tiff.serializeImageFile
