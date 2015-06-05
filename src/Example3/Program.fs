module Exmaple3

open System.Drawing
open System.Drawing.Imaging
open System.IO
open BitonalDocs.Rendering
open BitonalDocs.Compression
open BitonalDocs.Dithering

//-------------------------------------------------------------------------------------------------

let private render (g : Graphics) =

    g.PageUnit <- GraphicsUnit.Inch

    let assembly = System.Reflection.Assembly.GetExecutingAssembly()
    use stream = assembly.GetManifestResourceStream("Lenna.tiff")
    use image = Image.FromStream(stream)

    g.DrawImage(image, 0.00f, -2.50f)

let private writeOutput (image : Image) name =

    image.Save(@"..\..\..\dithering-" + name + ".png", ImageFormat.Png)

let private writeOriginal w h resolution render name =

    let resolution = single resolution
    let w = int (ceil (resolution * w))
    let h = int (ceil (resolution * h))

    use bitmap = new Bitmap(w, h, PixelFormat.Format24bppRgb)
    bitmap.SetResolution(resolution, resolution)

    use graphics = Graphics.FromImage(bitmap)
    render graphics

    writeOutput bitmap name

let private writeDithered w h resolution render (ditheringType, name) =

    let bytes = createTiffImage w h resolution render CompressionType.None ditheringType
    use stream = new MemoryStream(bytes)
    use image = Image.FromStream(stream)
    writeOutput image name

[<EntryPoint>]
let main argv =

    let w = 5.33f
    let h = 1.33f
    let resolution = 96

    let arguments =
        [ Threshold.fixed' 127uy,             "threshold-fixed-127"
          Threshold.dispersed8x8,             "threshold-dispersed8x8"
          Threshold.dispersed4x4,             "threshold-dispersed4x4"
          Threshold.dispersed2x2,             "threshold-dispersed2x2"
          Threshold.clustered8x8,             "threshold-clustered8x8"
          Threshold.clustered6x6,             "threshold-clustered6x6"
          Threshold.clustered4x4,             "threshold-clustered4x4"
          Threshold.clustered2x2,             "threshold-clustered2x2"
          ErrorDiffusion.basic,               "errorDiffusion-basic"
          ErrorDiffusion.falseFloydSteinberg, "errorDiffusion-falseFloydSteinberg"
          ErrorDiffusion.floydSteinberg,      "errorDiffusion-floydSteinberg"
          ErrorDiffusion.jarvisJudiceNinke,   "errorDiffusion-jarvisJudiceNinke"
          ErrorDiffusion.stucki,              "errorDiffusion-stucki"
          ErrorDiffusion.burkes,              "errorDiffusion-burkes"
          ErrorDiffusion.sierra3Row,          "errorDiffusion-sierra3Row"
          ErrorDiffusion.sierra2Row,          "errorDiffusion-sierra2Row"
          ErrorDiffusion.sierraLite,          "errorDiffusion-sierraLite"
          ErrorDiffusion.atkinson,            "errorDiffusion-atkinson"
          ErrorDiffusion.zhigangFan,          "errorDiffusion-zhigangFan"
          ErrorDiffusion.shiauFan1,           "errorDiffusion-shiauFan1"
          ErrorDiffusion.shiauFan2,           "errorDiffusion-shiauFan2" ]

    writeOriginal w h resolution render "original"
    writeDithered w h resolution render |> (fun x -> List.iter x arguments)
    0
