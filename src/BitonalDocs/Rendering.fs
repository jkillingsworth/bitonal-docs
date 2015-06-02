module BitonalDocs.Rendering

open System
open System.Drawing
open BitonalDocs.Imaging
open BitonalDocs.Compression
open BitonalDocs.Dithering
open BitonalDocs.Tiff

//-------------------------------------------------------------------------------------------------

let private compress = function

    | None
        -> convertPixelArrayToSeqPixels
        >> Seq.map (Array.map convertPixelToBit)
        >> Seq.map convertBitsTo1BppBytes
        >> Seq.concat
        >> Seq.toArray

    | Group3OneDimensional
        -> convertPixelArrayToSeqPixels
        >> Seq.map compressUsingGroup3
        >> Seq.map convertBitsTo1BppBytes
        >> Seq.concat
        >> Seq.toArray

    | PackBits
        -> convertPixelArrayToSeqPixels
        >> Seq.map (Array.map convertPixelToBit)
        >> Seq.map convertBitsTo1BppBytes
        >> Seq.map compressUsingPackBits
        >> Seq.concat
        >> Seq.toArray

let compression = function
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
